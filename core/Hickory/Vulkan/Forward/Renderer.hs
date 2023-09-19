{-# LANGUAGE DataKinds, OverloadedLists, OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts, OverloadedRecordDot #-}

module Hickory.Vulkan.Forward.Renderer where

import Hickory.Vulkan.Forward.Types (Renderer (..), castsShadow, DrawCommand (..), StaticConstants (..), MeshType (..), AnimatedMesh (..), AnimatedConstants (..), Command, MSDFMesh (..), RenderSettings (..), StaticMesh (..), DrawType (..), addCommand, CommandMonad, runCommand, highlightObjs, Globals(..), WorldGlobals (..), WorldSettings (..), CustomDrawCommand(..), Stage(..), OverlayGlobals (..), AllStageMaterial (..))
import Hickory.Vulkan.Vulkan ( mkAcquire)
import Acquire.Acquire (Acquire)
import Hickory.Vulkan.PostProcessing (withPostProcessMaterial)
import Linear (V4 (..), transpose, inv33, _m33, V2 (..), V3 (..), (!*!), inv44, (!*), _x, _y, _z, _w, (^/), distance, normalize, dot, cross, norm, Epsilon)
import Hickory.Vulkan.Monad (material, BufferedUniformMaterial (..), cmdDrawBufferedMesh, getMeshes, addMesh, askDynamicMesh, useDynamicMesh, DynamicMeshMonad, textMesh)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Hickory.Vulkan.Types (RenderTarget (..), DescriptorSpec (..), PointedDescriptorSet, buf, hasPerDrawDescriptorSet, Material(..), DeviceContext (..), VulkanResources (..), Swapchain, FrameContext (..), BufferedMesh (..), vertices, indices, DataBuffer (..), Mesh (..), Attribute)
import Hickory.Vulkan.Text (withMSDFMaterial, MSDFMatConstants (..), TextRenderer, withOverlayMSDFMaterial)
import Hickory.Vulkan.Forward.Lit (withStaticUnlitMaterial, withAnimatedLitMaterial, withLitRenderTarget, withStaticLitMaterial, withLineMaterial, withPointMaterial, withOverlayMaterial)
import Hickory.Vulkan.Forward.ShadowPass (withAnimatedShadowMaterial, withShadowRenderTarget, withStaticShadowMaterial, whiteFragShader)
import Hickory.Vulkan.RenderPass (withSwapchainRenderTarget, useRenderTarget)
import Hickory.Vulkan.Mesh (vsizeOf)
import Vulkan (ClearValue (..), ClearColorValue (..), cmdDraw, ClearDepthStencilValue (..), bindings, withDescriptorSetLayout, BufferUsageFlagBits (..), Extent2D (..), DescriptorSetLayout)
import Foreign (Storable, plusPtr, sizeOf)
import Hickory.Vulkan.DescriptorSet (withDescriptorSet, BufferDescriptorSet (..), descriptorSetBindings, withDataBuffer, uploadBufferDescriptor, uploadBufferDescriptorArray, withBufferDescriptorSet)
import Control.Lens (view, (^.), (.~), (&), _1, _2, _3, _4, _5, each, toListOf, has)
import Hickory.Vulkan.Framing (resourceForFrame, frameResource, withResourceForFrame, FramedResource)
import Hickory.Vulkan.Material (cmdBindMaterial, cmdPushMaterialConstants, cmdBindDrawDescriptorSet, PipelineOptions (..), withMaterial)
import Data.List (partition, sortOn, groupBy)
import Data.Foldable (for_)
import Hickory.Vulkan.DynamicMesh (DynamicBufferedMesh(..), withDynamicBufferedMesh)
import qualified Data.Vector as V
import Vulkan.Zero (zero)
import Hickory.Vulkan.Forward.ObjectPicking (withObjectIDMaterial, withObjectIDRenderTarget, ObjectIDConstants (..), withObjectHighlightMaterial)
import Linear.Matrix (M44)
import Hickory.Text.Types ( TextCommand(..) )
import Control.Monad (when, unless)
import Hickory.Types (Size (..))
import Hickory.Camera (cameraViewMat, cameraPos, cameraProjMat, cameraNear, cameraFar, Camera (..), Projection (..))
import Hickory.Math.Matrix ( viewDirection )
import Data.Word (Word32)
import Data.IORef (modifyIORef, newIORef, readIORef, IORef)
import Data.UUID (UUID)
import Control.Monad.State.Class ( MonadState, put, get )
import Control.Monad.State.Strict (evalStateT)
import qualified Data.UUID as UUID
import Data.Maybe (isJust, fromMaybe)
import Hickory.Vulkan.RenderTarget (copyDescriptorImageToBuffer, withImageBuffer, readPixel)
import Hickory.Math (Scalar, orthographicProjection, transformV3, glerp)
import Data.Fixed (div')
import Data.Traversable (for)
import VulkanMemoryAllocator (withMappedMemory)
import Control.Exception (bracket)
import Safe (headMay)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Proxy (Proxy)
import Data.UUID.V4 (nextRandom)

withRendererMaterial
  :: VulkanResources
  -> RenderTarget
  -> [Attribute]
  -> PipelineOptions
  -> B.ByteString
  -> B.ByteString
  -> [FramedResource PointedDescriptorSet]
  -> Maybe DescriptorSetLayout -- Per draw descriptor set
  -> Acquire (Material Word32)
withRendererMaterial vulkanResources renderTarget = withMaterial vulkanResources renderTarget (undefined :: Proxy Word32)

withAllStageMaterial
  :: forall uniform
  .  Storable uniform
  => VulkanResources
  -> Renderer
  -> PipelineOptions
  -> [Attribute]
  -> Maybe DescriptorSetLayout -- Per draw descriptor set
  -> ByteString
  -> ByteString
  -> ByteString
  -> ByteString
  -> ByteString
  -> Acquire (AllStageMaterial uniform)
withAllStageMaterial vulkanResources Renderer {..} pipelineOptions attributes perDrawLayout
  worldVertShader worldFragShader
  shadowVertShader objectIDVertShader objectIDFragShader = do
  uuid <- liftIO nextRandom
  descriptor <- frameResource $ withBufferDescriptorSet vulkanResources
  let uniformSize = sizeOf (undefined :: uniform)
      materialSet = view #descriptorSet <$> descriptor
      materialSets =
        [ globalDescriptorSet
        , materialSet
        ]

  worldMaterial         <- withRendererMaterial vulkanResources litRenderTarget attributes pipelineOptions worldVertShader worldFragShader materialSets perDrawLayout
  shadowMaterial        <- withRendererMaterial vulkanResources shadowRenderTarget attributes pipelineOptions { depthClampEnable = True } shadowVertShader whiteFragShader materialSets perDrawLayout
  objectIDMaterial      <- withRendererMaterial vulkanResources pickingRenderTarget attributes pipelineOptions { blendEnable = False } objectIDVertShader objectIDFragShader materialSets perDrawLayout
  showSelectionMaterial <- withRendererMaterial vulkanResources currentSelectionRenderTarget attributes pipelineOptions { blendEnable = False } objectIDVertShader objectIDFragShader materialSets perDrawLayout
  pure AllStageMaterial {..}

withRenderer :: VulkanResources -> Swapchain -> Acquire Renderer
withRenderer vulkanResources@VulkanResources {deviceContext = DeviceContext{..}} swapchain = do
  shadowRenderTarget       <- withShadowRenderTarget vulkanResources
  litRenderTarget          <- withLitRenderTarget vulkanResources swapchain
  swapchainRenderTarget    <- withSwapchainRenderTarget vulkanResources swapchain
  pickingRenderTarget          <- withObjectIDRenderTarget vulkanResources swapchain
  currentSelectionRenderTarget <- withObjectIDRenderTarget vulkanResources swapchain

  globalBuffer             <- frameResource $ withDataBuffer vulkanResources 1 BUFFER_USAGE_UNIFORM_BUFFER_BIT
  globalShadowPassBuffer   <- frameResource $ withDataBuffer vulkanResources 1 BUFFER_USAGE_UNIFORM_BUFFER_BIT
  globalWorldBuffer        <- frameResource $ withDataBuffer vulkanResources 1 BUFFER_USAGE_UNIFORM_BUFFER_BIT
  globalOverlayBuffer      <- frameResource $ withDataBuffer vulkanResources 1 BUFFER_USAGE_UNIFORM_BUFFER_BIT

  globalDescriptorSet <- for (V.zip5 globalBuffer globalWorldBuffer globalOverlayBuffer globalShadowPassBuffer (snd <$> shadowRenderTarget.frameBuffers))
    \(globalBuf, globalWorldBuf, globalOverlayBuf, globalShadowPassBuf, targetDescriptorSpecs) -> do
      withDescriptorSet vulkanResources $
        [ BufferDescriptor (buf globalBuf)
        , BufferDescriptor (buf globalWorldBuf)
        , BufferDescriptor (buf globalOverlayBuf)
        , BufferDescriptor (buf globalShadowPassBuf)
        ] ++ targetDescriptorSpecs

  -- For debugging
  shadowMapDescriptorSet <- for (snd <$> shadowRenderTarget.frameBuffers) $ withDescriptorSet vulkanResources

  imageSetLayout <- withDescriptorSetLayout device zero
    { bindings = V.fromList $ descriptorSetBindings [ImageDescriptor [error "Dummy image"]]
    } Nothing mkAcquire

  pickingMaterial          <- withObjectIDMaterial vulkanResources pickingRenderTarget globalDescriptorSet
  currentSelectionMaterial <- withObjectIDMaterial vulkanResources currentSelectionRenderTarget globalDescriptorSet

  staticShadowMaterial     <- withStaticShadowMaterial vulkanResources shadowRenderTarget globalDescriptorSet
  animatedShadowMaterial   <- withAnimatedShadowMaterial vulkanResources shadowRenderTarget globalDescriptorSet
  staticLitWorldMaterial   <- withStaticLitMaterial vulkanResources litRenderTarget globalDescriptorSet imageSetLayout
  staticUnlitWorldMaterial <- withStaticUnlitMaterial vulkanResources litRenderTarget globalDescriptorSet imageSetLayout
  animatedLitWorldMaterial <- withAnimatedLitMaterial vulkanResources litRenderTarget globalDescriptorSet imageSetLayout

  msdfWorldMaterial        <- withMSDFMaterial vulkanResources litRenderTarget globalDescriptorSet imageSetLayout
  linesWorldMaterial       <- withLineMaterial vulkanResources litRenderTarget globalDescriptorSet
  pointsWorldMaterial      <- withPointMaterial vulkanResources litRenderTarget globalDescriptorSet

  staticOverlayMaterial    <- withOverlayMaterial vulkanResources swapchainRenderTarget globalDescriptorSet imageSetLayout
  msdfOverlayMaterial      <- withOverlayMSDFMaterial vulkanResources swapchainRenderTarget globalDescriptorSet imageSetLayout

  postMaterialDescriptorSet <- for (snd <$> litRenderTarget.frameBuffers) $ withDescriptorSet vulkanResources

  objHighlightDescriptorSet <- for (snd <$> currentSelectionRenderTarget.frameBuffers) $ withDescriptorSet vulkanResources

  postProcessMaterial <- withPostProcessMaterial vulkanResources swapchainRenderTarget globalDescriptorSet postMaterialDescriptorSet
  objHighlightMaterial <- withObjectHighlightMaterial vulkanResources litRenderTarget globalDescriptorSet objHighlightDescriptorSet

  dynamicMesh <- frameResource $ withDynamicBufferedMesh vulkanResources 10000 -- For text, need 20 floats per non-whitespace character

  objectPickingImageBuffer <- withImageBuffer vulkanResources pickingRenderTarget 0

  pure Renderer {..}

data RegisteredMaterial const extra
  = NullMat
  | Universal (BufferedUniformMaterial const) extra
  | LitAndUnlit (BufferedUniformMaterial const, extra) (BufferedUniformMaterial const, extra)

isPointWithinCameraFrustum :: Scalar -> Camera -> V3 Scalar -> Bool
isPointWithinCameraFrustum _ Camera {projection = Ortho {}} _ = error "Ortho culling not yet implemented"
isPointWithinCameraFrustum ratio cam@Camera {projection = Perspective {..}, ..} p
  =  zInCamSpace > near
  && zInCamSpace < far
  && yInCamSpace < h/2
  && yInCamSpace > -h/2
  && xInCamSpace < w/2
  && xInCamSpace > -w/2
  where
  toP      = p - camP
  camP     = cameraPos cam
  camDir   = normalize angleVec
  camRight = normalize . negate $ normalize up `cross` camDir
  camUp    = normalize $ camRight `cross` camDir
  near     = cameraNear cam
  far      = cameraFar cam
  zInCamSpace = toP `dot` camDir
  yInCamSpace = toP `dot` camUp
  xInCamSpace = toP `dot` camRight
  h = zInCamSpace * 2 * tan (fov / 2)
  w = ratio * h

isSphereWithinCameraFrustum :: Scalar -> Camera -> V3 Scalar -> Scalar -> Bool
isSphereWithinCameraFrustum _ Camera {projection = Ortho {}} = error "Ortho culling not yet implemented"
isSphereWithinCameraFrustum ratio cam@Camera {projection = Perspective {..}, ..} = \p radius ->
  let toP = p - camP
      zInCamSpace = toP `dot` camDir
      yInCamSpace = toP `dot` camUp
      xInCamSpace = toP `dot` camRight

      h = zInCamSpace * 2 * tan (fov / 2)
      w = ratio * h
      vfov = fov
      hfov = atan (tan (fov / 2) / ratio) * 2
      horizontalSphereComponent = radius / cos (hfov/2)
      verticalSphereComponent   = radius / cos (vfov/2)
  in zInCamSpace + radius > near
  && zInCamSpace - radius < far
  && yInCamSpace < h/2 + verticalSphereComponent
  && yInCamSpace > -h/2 - verticalSphereComponent
  && xInCamSpace < w/2 + horizontalSphereComponent
  && xInCamSpace > -w/2 - horizontalSphereComponent
  where
  camP     = cameraPos cam
  camDir   = normalize angleVec
  camRight = normalize . negate $ normalize up `cross` camDir
  camUp    = normalize $ camRight `cross` camDir
  near     = cameraNear cam
  far      = cameraFar cam

sphereWithinCameraFrustumFromLightPerspective
 :: (Real a, Floating a, Epsilon a, Show a)
 => M44 a
 -> M44 a
 -> Extent2D
 -> V3 a
 -> V3 a
 -> V3 a
 -> V3 a
 -> a
 -> Bool
sphereWithinCameraFrustumFromLightPerspective viewProjMat lightView shadowMapExtent lightOrigin lightDir' lightUp' p radius
  =  pXInLightSpace + radius > l
  && pXInLightSpace - radius < r
  && pYInLightSpace - radius < b
  && pYInLightSpace + radius > t
  where
  -- toP = p - lightOrigin
  -- lightDir = normalize lightDir'
  -- lightRight = normalize $ negate (normalize lightUp' `cross` lightDir)
  -- lightUp    = normalize $ lightRight `cross` lightDir
  -- pYInLightSpace = lightUp `dot` toP
  -- pXInLightSpace = lightRight `dot` toP
  (V3 pXInLightSpace pYInLightSpace _) = transformV3 lightView p
  (l, r, b, t, _n, _f) = viewFrustumBoundaryInLightSpace viewProjMat lightView shadowMapExtent

ndcBoundaryPoints :: Num a => [V4 a]
ndcBoundaryPoints =
  [ V4 (-1) (-1) 0 1
  , V4 (-1)   1  0 1
  , V4   1    1  0 1
  , V4   1  (-1) 0 1
  , V4 (-1) (-1) 1 1
  , V4 (-1)   1  1 1
  , V4   1    1  1 1
  , V4   1  (-1) 1 1
  ]

viewBoundaryFromInvProjection :: forall a. (Fractional a, Num a, Ord a) => M44 a -> (a, a, a, a, a, a)
viewBoundaryFromInvProjection m =  (l,r,b,t,n,f)
  where
  frustumPoints = (\v -> v ^/ (v ^. _w)) . (m !*) <$> ndcBoundaryPoints
  l = minimum . fmap (^. _x) $ frustumPoints
  r = maximum . fmap (^. _x) $ frustumPoints
  b = maximum . fmap (^. _y) $ frustumPoints
  t = minimum . fmap (^. _y) $ frustumPoints
  n = minimum . fmap (^. _z) $ frustumPoints
  f = maximum . fmap (^. _z) $ frustumPoints

-- Given a matrix going from NDC into light space (for the camera), give the boundaries of the shadow map volume
viewFrustumBoundaryInLightSpace :: (Show a, Real a, Floating a, Ord a) => M44 a -> M44 a -> Extent2D -> (a,a,a,a,a,a)
viewFrustumBoundaryInLightSpace viewProjMat lightView shadowMapExtent = (l', r', b', t', n', f') -- TODO: Near plane should take into account objects outside the camera
  where
  -- Get a transform from NDC to light space
  invvp = inv44 viewProjMat
  ndcToLightSpace = lightView !*! invvp
  -- Find the camera frustum in light space
  frustumPoints = xformPoint ndcToLightSpace <$> ndcBoundaryPoints
  -- Find the bounding box of the frustum
  l = minimum . fmap (^. _x) $ frustumPoints
  -- r = maximum . fmap (^. _x) $ frustumPoints
  -- b = maximum . fmap (^. _y) $ frustumPoints
  t = minimum . fmap (^. _y) $ frustumPoints
  n = minimum . fmap (^. _z) $ frustumPoints
  -- f = maximum . fmap (^. _z) $ frustumPoints
  -- To eliminate visual artifacts from rotating the camera, the bounding
  -- box should always be the same size, no matter how you look at it
  [p0, _p1, _p2, _p3, p4, _p5, p6, _p7] = frustumPoints
  -- What's the longest diagonal of the frustum? Either it's a diagonal
  -- from the near to far plane, or a diagonal of the far plane
  diag1 = distance p0 p6 -- from one corner of the near plane, to the opposite corner of the far plane
  diag2 = distance p4 p6 -- from one corner of the far plane, to the opposite corner of the far plane
  shadowMapWidth = realToFrac shadowMapExtent.width
  longest = max diag1 diag2
          -- We're going to snap the boundary position to texel increments
          -- of the shadow map, so increase the size by a texel to have
          -- room at the boundary
          * (shadowMapWidth/(shadowMapWidth-1))
  -- Snap position to the nearest texel
  l' = snap (longest / shadowMapWidth) l
  t' = snap (longest / shadowMapWidth) t
  n' = snap (longest / shadowMapWidth) n
  -- Bump out the boundaries to fit the maximum diagnoal
  r' = l' + longest
  b' = t' + longest
  f' = n' + longest

  xformPoint m = (\v -> v ^/ (v ^. _w)) . (m !*)
  snap by x = let r = x `div'` by in realToFrac (r :: Int) * by

lightProjection :: (Show a, Real a, Floating a, Ord a) => M44 a -> M44 a -> Extent2D -> M44 a
lightProjection viewProjMat lightView shadowMapExtent = orthographicProjection l' r' b' t' n' f'
  where
  (l', r', b', t', n', f') = viewFrustumBoundaryInLightSpace viewProjMat lightView shadowMapExtent

boundingSphere :: CustomDrawCommand -> (V3 Float, Float)
boundingSphere CustomDrawCommand {..} = (center, norm (max' - center))
  where
  Mesh {..} = case mesh of
    Dynamic m -> m
    Buffered m -> m.mesh
  min' = transformV3 modelMat minPosition
  max' = transformV3 modelMat maxPosition
  center = glerp 0.5 min' max'

renderToRenderer :: (MonadIO m) => FrameContext -> Renderer -> RenderSettings -> Command () -> Command () -> m ()
renderToRenderer frameContext@FrameContext{..} Renderer {..} RenderSettings {..} litF overlayF = do
  useDynamicMesh (resourceForFrame swapchainImageIndex dynamicMesh) do
    let WorldSettings {..} = worldSettings
        Extent2D w h = extent
        lightOrigin = V3 0 0 0
        lightUp = V3 0 0 1
        lightView = viewDirection lightOrigin lightDirection lightUp -- Trying to get the whole scene in view of the sun
        projMat = cameraProjMat (Size (fromIntegral w) (fromIntegral h)) camera
        viewMat = cameraViewMat camera
        viewProjMat = projMat !*! viewMat
        lightProj = lightProjection viewProjMat lightView (shadowRenderTarget.extent)
        nearPlane = cameraNear camera
        farPlane = cameraFar camera
        worldGlobals = WorldGlobals { camPos = cameraPos camera, multiSampleCount = fromIntegral multiSampleCount, ..}
        testSphereInCameraFrustum = isSphereWithinCameraFrustum (realToFrac w / realToFrac h) camera
        testSphereInShadowFrustum = sphereWithinCameraFrustumFromLightPerspective viewProjMat lightView (shadowRenderTarget.extent) lightOrigin lightDirection lightUp
        worldCullTest cdc    = not cdc.overlay && (not cdc.cull || uncurry testSphereInCameraFrustum (boundingSphere cdc))
        pickingCullTest cdc  = not cdc.overlay && isJust cdc.hasIdent && (not cdc.cull || uncurry testSphereInCameraFrustum (boundingSphere cdc))
        shadowCullTest cdc   = not cdc.overlay && cdc.doCastShadow && (not cdc.cull || uncurry testSphereInShadowFrustum (boundingSphere cdc))
        overlayCullTest cdc  = cdc.overlay
        showSelectionCullTest cdc = isJust cdc.hasIdent && (cdc.hasIdent `elem` fmap Just highlightObjs) && worldCullTest cdc
        shadowPassGlobals = OverlayGlobals lightView lightProj

    withResourceForFrame swapchainImageIndex globalBuffer \buf ->
      uploadBufferDescriptor buf
        $ Globals frameNumber

    withResourceForFrame swapchainImageIndex globalWorldBuffer \buf ->
      uploadBufferDescriptor buf
        $ worldGlobals
        & #lightTransform .~ (lightProj !*! lightView)

    withResourceForFrame swapchainImageIndex globalShadowPassBuffer $
      flip uploadBufferDescriptor shadowPassGlobals

    withResourceForFrame swapchainImageIndex globalOverlayBuffer $
      flip uploadBufferDescriptor overlayGlobals

    let drawCommands = runCommand litF
        nonCustomCommands = filter (not . has #_Custom) drawCommands
        -- id all commands
        customDrawCommands = zip [0..] $ toListOf (each . #_Custom) drawCommands

        -- group by material and give uniform numbers
        grouped = groupBy (\(_, CustomDrawCommand { material = m1 }) (_,CustomDrawCommand { material = m2 }) -> m1.uuid == m2.uuid) customDrawCommands

    groupedByMaterial :: [[(Int, Word32, CustomDrawCommand)]] <- for grouped \group -> case headMay group of
      Nothing -> pure []
      Just (_, CustomDrawCommand { material }) -> do
        let BufferDescriptorSet { dataBuffer } = resourceForFrame swapchainImageIndex material.descriptor
            DataBuffer {..} = dataBuffer
        liftIO $ withMappedMemory allocator allocation bracket \bufptr -> do
          for (zip group [0..]) \((i, cdc@CustomDrawCommand {pokeData}), uniIdx) -> do
            liftIO $ pokeData (plusPtr bufptr (material.uniformSize * uniIdx))
            pure (i, fromIntegral uniIdx, cdc)

    -- map from id to uniform number
    let idToUni = HashMap.fromList $ (\(i,uni,_) -> (i,uni)) <$> concat groupedByMaterial
        blended = (\(i, cdc) -> (fromMaybe 0 $ HashMap.lookup i idToUni, cdc)) <$> filter (\(_, cdc) -> cdc.doBlend) customDrawCommands
        allGrouped = fmap (\(_, uni, cdc) -> (uni,cdc)) <$> groupedByMaterial
        opaqueGrouped = filter (\(_, cdc) -> not cdc.doBlend) <$> allGrouped

    useRenderTarget pickingRenderTarget commandBuffer [ DepthStencil (ClearDepthStencilValue 1 0), Color (Uint32 0 0 0 0) ] swapchainImageIndex do
      processIDRenderPass frameContext (Universal pickingMaterial ()) $ filter (isJust . ident) nonCustomCommands
      processCustomCommandGroups frameContext Picking (filter (pickingCullTest . snd) <$> allGrouped)

    useRenderTarget currentSelectionRenderTarget commandBuffer [ DepthStencil (ClearDepthStencilValue 1 0), Color (Uint32 0 0 0 0) ] swapchainImageIndex do
      processIDRenderPass frameContext (Universal currentSelectionMaterial ()) $ filter ((\x -> x `elem` map Just highlightObjs) . ident) nonCustomCommands
      processCustomCommandGroups frameContext ShowSelection (filter (showSelectionCullTest . snd) <$> allGrouped)

    useRenderTarget shadowRenderTarget commandBuffer [ DepthStencil (ClearDepthStencilValue 1 0) ] swapchainImageIndex do
      processRenderPass frameContext
        ( Universal animatedShadowMaterial ()
        , Universal staticShadowMaterial ()
        , NullMat
        , NullMat
        , NullMat
        ) $ filter castsShadow nonCustomCommands
      processCustomCommandGroups frameContext ShadowMap (filter (shadowCullTest . snd) <$> allGrouped)

    let V4 r g b a = clearColor
    useRenderTarget litRenderTarget commandBuffer
      [ Color (Float32 r g b a)
      , DepthStencil (ClearDepthStencilValue 1 0)
      , Color (Float32 1 1 1 1)
      ] swapchainImageIndex do
      processRenderPass frameContext
        ( Universal animatedLitWorldMaterial ()
        , LitAndUnlit (staticLitWorldMaterial, ()) (staticUnlitWorldMaterial, ())
        , Universal msdfWorldMaterial ()
        , Universal linesWorldMaterial ()
        , Universal pointsWorldMaterial ()
        ) nonCustomCommands
      processCustomCommandUngrouped frameContext World (filter (worldCullTest . snd) blended)
      processCustomCommandGroups frameContext World (filter (worldCullTest . snd) <$> opaqueGrouped)

      case highlightObjs of
        (_:_) -> do
          cmdBindMaterial frameContext objHighlightMaterial
          liftIO $ cmdDraw commandBuffer 3 1 0 0
        _ -> pure ()

    useRenderTarget swapchainRenderTarget commandBuffer [] swapchainImageIndex do
      cmdBindMaterial frameContext postProcessMaterial
      liftIO $ cmdPushMaterialConstants commandBuffer postProcessMaterial postSettings
      liftIO $ cmdDraw commandBuffer 3 1 0 0

      processRenderPass frameContext
        ( NullMat
        , Universal staticOverlayMaterial ()
        , Universal msdfOverlayMaterial ()
        , NullMat
        , NullMat
        ) $ runCommand overlayF
      processCustomCommandGroups frameContext Overlay (filter (overlayCullTest . snd) <$> opaqueGrouped)
      processCustomCommandUngrouped frameContext Overlay (filter (overlayCullTest . snd) blended)
    liftIO $ copyDescriptorImageToBuffer commandBuffer (resourceForFrame swapchainImageIndex objectPickingImageBuffer)

type MaterialConfig c = RegisteredMaterial c (IORef [c])

regMatToConfig :: RegisteredMaterial a () -> IO (MaterialConfig a)
regMatToConfig = \case
  NullMat -> pure NullMat
  Universal mat () -> Universal mat <$> newIORef []
  LitAndUnlit (mat1,()) (mat2,()) -> LitAndUnlit <$> ((mat1,) <$> newIORef [])
                                                 <*> ((mat2,) <$> newIORef [])

uploadUniforms :: (Storable a, MonadIO m) => FrameContext -> MaterialConfig a -> m ()
uploadUniforms fc = \case
  Universal mat ref -> uploadConfig (mat,ref)
  LitAndUnlit c1 c2 -> uploadConfig c1 >> uploadConfig c2
  NullMat -> pure ()
  where
  uploadConfig (mat, ref) = do
    unis <- liftIO (readIORef ref)
    unless (null unis) do
      uploadUniformsBuffer fc mat $ reverse unis

submitCommand :: (MonadIO m, DynamicMeshMonad m, MonadState UUID m) => FrameContext -> DrawCommand -> MaterialConfig a -> (a, Maybe PointedDescriptorSet) -> m ()
submitCommand _ (Custom _) _ _ = error "Custom not supported here"
submitCommand frameContext DrawCommand{..} c (uniform, ds) = do
  let config = case c of
        Universal mat ref -> (mat, ref)
        LitAndUnlit c1 c2 -> if lit then c1 else c2
        NullMat -> error "No mat registered to handle this draw command"
  unis <- liftIO $ readIORef (snd config)
  bindMaterialIfNeeded frameContext (fst config).material
  renderCommand frameContext (fst config).material mesh ds (fromIntegral $ length unis)
  liftIO $ modifyIORef (snd config) (uniform:)

bindMaterialIfNeeded :: (MonadState UUID m, MonadIO m) => FrameContext -> Material Word32 -> m ()
bindMaterialIfNeeded fc material = do
  curUUID <- get
  when (curUUID /= material.uuid) do
    cmdBindMaterial fc material
    put curUUID

renderCommand
  :: (MonadIO m, DynamicMeshMonad m)
  => FrameContext
  -> Material Word32
  -> MeshType
  -> Maybe PointedDescriptorSet
  -> Word32
  -> m ()
renderCommand FrameContext {..} material mesh drawDS uniformIdx = do
  cmdPushMaterialConstants commandBuffer material uniformIdx
  when (hasPerDrawDescriptorSet material) do
    for_ drawDS $ cmdBindDrawDescriptorSet commandBuffer material
  case mesh of
    Buffered BufferedMesh {mesh = mesh', ..} -> cmdDrawBufferedMesh commandBuffer material mesh' 0 vertexBuffer 0 indexBuffer
    Dynamic dyn -> do
      meshes <- getMeshes
      addMesh dyn

      -- This is O(n)... Might want to cache this
      let vertexSizeThusFar = sum $ map (sum . map (vsizeOf . snd) . vertices) meshes
          indexSizeThusFar  = sum $ map (maybe 0 vsizeOf . indices) meshes

      DynamicBufferedMesh { vertexBufferPair = (vertexBuffer,_), indexBufferPair = (indexBuffer,_) } <- askDynamicMesh
      cmdDrawBufferedMesh commandBuffer material dyn vertexSizeThusFar vertexBuffer (fromIntegral indexSizeThusFar) (Just indexBuffer)

uploadUniformsBuffer :: (MonadIO m, Storable a) => FrameContext -> BufferedUniformMaterial a -> [a] -> m ()
uploadUniformsBuffer FrameContext {..} BufferedUniformMaterial {..} uniforms = do
  let BufferDescriptorSet { dataBuffer } = resourceForFrame swapchainImageIndex descriptor

  uploadBufferDescriptorArray dataBuffer uniforms

drawText
  :: CommandMonad m
  => TextRenderer
  -> M44 Float
  -> V4 Float
  -> V4 Float
  -> Float
  -> TextCommand
  -> m ()
drawText (font, fontTex, sdfPixelRange) mat color outlineColor outlineSize tc =
  addCommand $ DrawCommand
    { modelMat = mat
    , mesh = Dynamic (textMesh font tc)
    , color = color
    , drawType = MSDF $ MSDFMesh fontTex outlineColor outlineSize sdfPixelRange (V2 1 1)
    , lit = False
    , castsShadow = False
    , blend = True
    , ident = Nothing
    , specularity = 8
    }

-- Main Command Processor

type MaterialSet
  = ( RegisteredMaterial AnimatedConstants ()
    , RegisteredMaterial StaticConstants ()
    , RegisteredMaterial MSDFMatConstants ()
    , RegisteredMaterial StaticConstants ()
    , RegisteredMaterial StaticConstants ()
    )

type MaterialConfigSet
  = ( MaterialConfig AnimatedConstants
    , MaterialConfig StaticConstants
    , MaterialConfig MSDFMatConstants
    , MaterialConfig StaticConstants
    , MaterialConfig StaticConstants
    )

processRenderPass
  :: (MonadIO m, DynamicMeshMonad m)
  => FrameContext
  -> MaterialSet
  -> [DrawCommand]
  -> m ()
processRenderPass fc mps commands = do
  config <- liftIO $
    (,,,,) <$> regMatToConfig (view _1 mps)
           <*> regMatToConfig (view _2 mps)
           <*> regMatToConfig (view _3 mps)
           <*> regMatToConfig (view _4 mps)
           <*> regMatToConfig (view _5 mps)

  let (blended, opaque) = partition blend commands

  flip evalStateT UUID.nil do
    -- Sort opaque by material to minimize binding
    for_ (sortOn identifyDCMat opaque) $ processCommand fc config
    -- Render blended in order to maintain visual layering TODO: Should sort by distance to camera instead
    for_ blended $ processCommand fc config

  -- IORefs are now bursting full of uniform goodness. Here comes the airplane. Yum!
  uploadUniforms fc $ view _1 config
  uploadUniforms fc $ view _2 config
  uploadUniforms fc $ view _3 config
  uploadUniforms fc $ view _4 config
  uploadUniforms fc $ view _5 config
  where
  identifyDCMat :: DrawCommand -> Int
  identifyDCMat (Custom _) = error "Custom not supported here"
  identifyDCMat DrawCommand {..} = (if lit then 2 else 1) * case drawType of
    Animated _ -> 5
    Static   _ -> 4
    MSDF     _ -> 3
    Points     -> 2
    Lines      -> 1 -- Draw lines first. Might need to be configurable

processCommand
  :: (MonadIO m, DynamicMeshMonad m, MonadState UUID m)
  => FrameContext
  -> MaterialConfigSet
  -> DrawCommand
  -> m ()
processCommand _ _ (Custom _) = error "Custom not supported here"
processCommand frameContext
  ( animatedConfig
  , staticConfig
  , msdfConfig
  , linesConfig
  , pointsConfig
  )
  dc@DrawCommand {..} = case drawType of
  Animated AnimatedMesh {..} -> submitCommand frameContext dc animatedConfig (AnimatedConstants
    { modelMat  = modelMat
    , normalMat = transpose . inv33 $ modelMat ^. _m33
    , color
    , boneMat
    , colors
    , specularity
    }, Just albedo)
  Static StaticMesh {..} -> submitCommand frameContext dc staticConfig (StaticConstants
    { modelMat  = modelMat
    , normalMat = transpose . inv33 $ modelMat ^. _m33
    , color = color
    , tiling = tiling
    , specularity
    }, Just albedo)
  Lines -> submitCommand frameContext dc linesConfig (StaticConstants
    { modelMat  = modelMat
    , normalMat = transpose . inv33 $ modelMat ^. _m33
    , color = color
    , tiling = V2 1 1
    , specularity
    }, Nothing)
  Points -> submitCommand frameContext dc pointsConfig (StaticConstants
    { modelMat  = modelMat
    , normalMat = transpose . inv33 $ modelMat ^. _m33
    , color = color
    , tiling = V2 1 1
    , specularity
    }, Nothing)
  MSDF MSDFMesh {..} -> submitCommand frameContext dc msdfConfig (MSDFMatConstants
    { modelMat  = modelMat
    , outlineColor = outlineColor
    , outlineSize = outlineSize
    , sdfPixelRange = sdfPixelRange
    , tiling = tiling
    , color = color
    }, Just tex)

-- Object ID Command Processor

processIDRenderPass
  :: (MonadIO m, DynamicMeshMonad m)
  => FrameContext
  -> RegisteredMaterial ObjectIDConstants ()
  -> [DrawCommand]
  -> m ()
processIDRenderPass fc mps commands = do
  config <- liftIO $ regMatToConfig mps

  flip evalStateT UUID.nil do
    for_ commands $ processIDCommand fc config

  -- IORefs are now bursting full of uniform goodness. Here comes the airplane. Yum!
  uploadUniforms fc config

processIDCommand
  :: (MonadIO m, DynamicMeshMonad m, MonadState UUID m)
  => FrameContext
  -> MaterialConfig ObjectIDConstants
  -> DrawCommand
  -> m ()
processIDCommand _ _ (Custom _) = error "Custom not supported here"
processIDCommand frameContext
  objectConfig
  dc@DrawCommand {..} = case ident of
    Just i ->
      go objectConfig $ (,Nothing) ObjectIDConstants
        { modelMat = modelMat
        , objectID = fromIntegral i
        }
    _ -> pure ()

  where
  go = submitCommand frameContext dc

pickObjectID :: FrameContext -> Renderer -> (Scalar,Scalar) -> IO Int
pickObjectID FrameContext {..} Renderer{..} = fmap fromIntegral . readPixel (resourceForFrame (swapchainImageIndex - 1) objectPickingImageBuffer)

-- Custom Commands
processCustomCommandGroups
  :: (MonadIO m, DynamicMeshMonad m)
  => FrameContext
  -> Stage
  -> [[(Word32, CustomDrawCommand)]]
  -> m ()
processCustomCommandGroups fc stage grouped = do
  for_ grouped \group ->
    case headMay group of
      Nothing -> pure ()
      Just (_, CustomDrawCommand { material }) -> do
        let theMat = case stage of
              World -> material.worldMaterial
              ShadowMap -> material.shadowMaterial
              Picking -> material.objectIDMaterial
              Overlay -> error "Overlay not supported yet"
              ShowSelection -> material.showSelectionMaterial
        cmdBindMaterial fc theMat
        for_ group \(i, CustomDrawCommand {mesh, descriptorSet}) -> do
          -- if stage == _
          renderCommand fc theMat mesh descriptorSet i

processCustomCommandUngrouped
  :: (MonadIO m, DynamicMeshMonad m)
  => FrameContext
  -> Stage
  -> [(Word32, CustomDrawCommand)]
  -> m ()
processCustomCommandUngrouped fc stage commands = do
  flip evalStateT UUID.nil do
    for_ commands \(i, CustomDrawCommand {mesh, descriptorSet, material}) -> do
      let theMat = case stage of
            World -> material.worldMaterial
            ShadowMap -> material.shadowMaterial
            Picking -> material.objectIDMaterial
            Overlay -> error "Overlay not supported yet"
            ShowSelection -> material.showSelectionMaterial
      bindMaterialIfNeeded fc theMat
      renderCommand fc theMat mesh descriptorSet (fromIntegral i)
