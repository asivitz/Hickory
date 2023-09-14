{-# LANGUAGE DataKinds, OverloadedLists, OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts, OverloadedRecordDot #-}

module Hickory.Vulkan.Forward.Renderer where

import Hickory.Vulkan.Forward.Types (Renderer (..), castsShadow, DrawCommand (..), StaticConstants (..), MeshType (..), AnimatedMesh (..), AnimatedConstants (..), Command, MSDFMesh (..), RenderSettings (..), StaticMesh (..), DrawType (..), addCommand, CommandMonad, runCommand, highlightObjs, Globals(..), WorldGlobals (..), WorldSettings (..), CustomDrawCommand(..), Stage(..))
import Hickory.Vulkan.Vulkan ( mkAcquire)
import Acquire.Acquire (Acquire)
import Hickory.Vulkan.PostProcessing (withPostProcessMaterial)
import Linear (V4 (..), transpose, inv33, _m33, V2 (..), V3 (..), (!*!), inv44, (!*), _x, _y, _z, _w, (^/), distance, normalize, dot, cross, norm, Epsilon)
import Hickory.Vulkan.Monad (material, BufferedUniformMaterial (..), cmdDrawBufferedMesh, getMeshes, addMesh, askDynamicMesh, useDynamicMesh, DynamicMeshMonad, textMesh)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Hickory.Vulkan.Types (RenderTarget (..), DescriptorSpec (..), PointedDescriptorSet, buf, hasPerDrawDescriptorSet, Material(..), DeviceContext (..), VulkanResources (..), Swapchain, FrameContext (..), BufferedMesh (..), vertices, indices, DataBuffer (..), Mesh (..))
import Hickory.Vulkan.Text (withMSDFMaterial, MSDFMatConstants (..), TextRenderer)
import Hickory.Vulkan.Forward.Lit (withStaticUnlitMaterial, withAnimatedLitMaterial, withLitRenderTarget, withStaticLitMaterial, withLineMaterial, withPointMaterial)
import Hickory.Vulkan.Forward.ShadowPass (withAnimatedShadowMaterial, withShadowRenderTarget, withStaticShadowMaterial)
import Hickory.Vulkan.RenderPass (withSwapchainRenderTarget, useRenderTarget)
import Hickory.Vulkan.Mesh (vsizeOf)
import Vulkan (ClearValue (..), ClearColorValue (..), cmdDraw, ClearDepthStencilValue (..), bindings, withDescriptorSetLayout, BufferUsageFlagBits (..), Extent2D (..))
import Foreign (Storable, plusPtr)
import Hickory.Vulkan.DescriptorSet (withDescriptorSet, BufferDescriptorSet (..), descriptorSetBindings, withDataBuffer, uploadBufferDescriptor, uploadBufferDescriptorArray)
import Control.Lens (view, (^.), (.~), (&), _1, _2, _3, _4, _5, each, toListOf, zoom, (%=))
import Hickory.Vulkan.Framing (resourceForFrame, frameResource, withResourceForFrame)
import Hickory.Vulkan.Material (cmdBindMaterial, cmdPushMaterialConstants, cmdBindDrawDescriptorSet)
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
import Control.Monad.State.Strict (evalStateT, lift)
import qualified Data.UUID as UUID
import Data.Maybe (isJust, mapMaybe, fromMaybe)
import Hickory.Vulkan.RenderTarget (copyDescriptorImageToBuffer, withImageBuffer, readPixel)
import Hickory.Math (Scalar, orthographicProjection, transformV3, glerp)
import Data.Fixed (div')
import Data.Traversable (for)
import VulkanMemoryAllocator (withMappedMemory)
import Control.Exception (bracket)
import Safe (headMay)
import qualified Data.HashMap.Strict as HashMap

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

  globalWorldDescriptorSet <- for (V.zip3 globalBuffer globalWorldBuffer (snd <$> shadowRenderTarget.frameBuffers))
    \(globalBuf, globalWorldBuf, targetDescriptorSpecs) -> do
      withDescriptorSet vulkanResources $
        [ BufferDescriptor (buf globalBuf)
        , BufferDescriptor (buf globalWorldBuf)
        ] ++ targetDescriptorSpecs

  globalOverlayDescriptorSet <- for (V.zip globalBuffer globalOverlayBuffer) \(globalBuf, overlayBuf) ->
    withDescriptorSet vulkanResources
      [ BufferDescriptor (buf globalBuf)
      , BufferDescriptor (buf overlayBuf)
      ]

  globalShadowPassDescriptorSet <- for (V.zip globalBuffer globalShadowPassBuffer) \(globalBuf, shadowBuf) ->
    withDescriptorSet vulkanResources
      [ BufferDescriptor (buf globalBuf)
      , BufferDescriptor (buf shadowBuf)
      ]

  -- For debugging
  shadowMapDescriptorSet <- for (snd <$> shadowRenderTarget.frameBuffers) $ withDescriptorSet vulkanResources

  imageSetLayout <- withDescriptorSetLayout device zero
    { bindings = V.fromList $ descriptorSetBindings [ImageDescriptor [error "Dummy image"]]
    } Nothing mkAcquire

  pickingMaterial          <- withObjectIDMaterial vulkanResources pickingRenderTarget globalWorldDescriptorSet
  currentSelectionMaterial <- withObjectIDMaterial vulkanResources currentSelectionRenderTarget globalWorldDescriptorSet

  staticShadowMaterial     <- withStaticShadowMaterial vulkanResources shadowRenderTarget globalShadowPassDescriptorSet
  animatedShadowMaterial   <- withAnimatedShadowMaterial vulkanResources shadowRenderTarget globalShadowPassDescriptorSet
  staticLitWorldMaterial   <- withStaticLitMaterial vulkanResources litRenderTarget globalWorldDescriptorSet imageSetLayout
  staticUnlitWorldMaterial <- withStaticUnlitMaterial vulkanResources litRenderTarget globalWorldDescriptorSet imageSetLayout
  animatedLitWorldMaterial <- withAnimatedLitMaterial vulkanResources litRenderTarget globalWorldDescriptorSet imageSetLayout

  msdfWorldMaterial        <- withMSDFMaterial vulkanResources litRenderTarget globalWorldDescriptorSet imageSetLayout
  linesWorldMaterial       <- withLineMaterial vulkanResources litRenderTarget globalWorldDescriptorSet
  pointsWorldMaterial      <- withPointMaterial vulkanResources litRenderTarget globalWorldDescriptorSet

  staticOverlayMaterial    <- withStaticUnlitMaterial vulkanResources swapchainRenderTarget globalOverlayDescriptorSet imageSetLayout
  msdfOverlayMaterial      <- withMSDFMaterial vulkanResources swapchainRenderTarget globalOverlayDescriptorSet imageSetLayout


  postMaterialDescriptorSet <- for (snd <$> litRenderTarget.frameBuffers) $ withDescriptorSet vulkanResources

  objHighlightDescriptorSet <- for (snd <$> currentSelectionRenderTarget.frameBuffers) $ withDescriptorSet vulkanResources

  postProcessMaterial <- withPostProcessMaterial vulkanResources swapchainRenderTarget globalWorldDescriptorSet postMaterialDescriptorSet
  objHighlightMaterial <- withObjectHighlightMaterial vulkanResources litRenderTarget globalWorldDescriptorSet objHighlightDescriptorSet

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
  camP = cameraPos cam
  toP = p - camP
  camDir   = normalize angleVec
  camUp    = normalize up
  camRight = negate $ camUp `cross` camDir
  zInCamSpace = toP `dot` camDir
  yInCamSpace = toP `dot` camUp
  xInCamSpace = toP `dot` camRight
  near = cameraNear cam
  far = cameraFar cam
  h = zInCamSpace * 2 * tan (fov / 2)
  w = h * ratio

isSphereWithinCameraFrustum :: Scalar -> Camera -> V3 Scalar -> Scalar -> Bool
isSphereWithinCameraFrustum _ Camera {projection = Ortho {}} = error "Ortho culling not yet implemented"
isSphereWithinCameraFrustum ratio cam@Camera {projection = Perspective {..}, ..} = \p radius ->
  let toP = p - camP
      zInCamSpace = toP `dot` camDir
      yInCamSpace = toP `dot` camUp
      xInCamSpace = toP `dot` camRight
      h = zInCamSpace * 2 * tan (fov / 2)
      w = h * ratio
      horizontalSphereComponent = radius / cos horizontalViewAngle
      verticalSphereComponent   = radius / cos (atan(tan horizontalViewAngle * ratio))
  in zInCamSpace + radius > near
  && zInCamSpace - radius < far
  && yInCamSpace - verticalSphereComponent < h/2
  && yInCamSpace + verticalSphereComponent > -h/2
  && xInCamSpace - horizontalSphereComponent < w/2
  && xInCamSpace + horizontalSphereComponent > -w/2
  where
  camP = cameraPos cam
  camDir   = normalize angleVec
  camUp    = normalize up
  camRight = negate $ camUp `cross` camDir
  near = cameraNear cam
  far = cameraFar cam

  horizontalViewAngle       = fov/2

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
sphereWithinCameraFrustumFromLightPerspective viewProjMat lightView shadowMapExtent lightOrigin lightDir lightUp p radius
  =  pXInLightSpace + radius > l
  && pXInLightSpace - radius < r
  && pYInLightSpace + radius > b
  && pYInLightSpace - radius < t
  where
  toP = p - lightOrigin
  lightRight = negate (lightUp `cross` normalize lightDir)
  pYInLightSpace = lightUp `dot` toP
  pXInLightSpace = lightRight `dot` toP
  (l, r, b, t, n, f) = viewFrustumBoundaryInLightSpace viewProjMat lightView shadowMapExtent

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
        worldCullTest cdc  = not cdc.cull || uncurry testSphereInCameraFrustum (boundingSphere cdc)
        shadowCullTest cdc = not cdc.cull || uncurry testSphereInShadowFrustum (boundingSphere cdc)
        overlayCullTest _ = True

    withResourceForFrame swapchainImageIndex globalBuffer \buf ->
      uploadBufferDescriptor buf
        $ Globals frameNumber

    withResourceForFrame swapchainImageIndex globalWorldBuffer \buf ->
      uploadBufferDescriptor buf
        $ worldGlobals
        & #lightTransform .~ (lightProj !*! lightView)

    withResourceForFrame swapchainImageIndex globalShadowPassBuffer \buf ->
      uploadBufferDescriptor buf
        $ worldGlobals
        & #viewMat .~ lightView
        & #projMat .~ lightProj
      -- TODO: Dynamic based on camera pos/target

    withResourceForFrame swapchainImageIndex globalOverlayBuffer \buf ->
      uploadBufferDescriptor buf $ worldGlobals
                                 & #viewMat .~ (overlayGlobals ^. #viewMat)
                                 & #projMat .~ (overlayGlobals ^. #projMat)

    let drawCommands = runCommand litF
        customDrawCommands = toListOf (each . #_Custom) drawCommands

    useRenderTarget pickingRenderTarget commandBuffer [ DepthStencil (ClearDepthStencilValue 1 0), Color (Uint32 0 0 0 0) ] swapchainImageIndex do
      processIDRenderPass frameContext (Universal pickingMaterial ()) $ filter (isJust . ident) drawCommands
      processCustomCommands frameContext (filter (\x -> x.stage == Picking && worldCullTest x) customDrawCommands)

    useRenderTarget currentSelectionRenderTarget commandBuffer [ DepthStencil (ClearDepthStencilValue 1 0), Color (Uint32 0 0 0 0) ] swapchainImageIndex do
      processIDRenderPass frameContext (Universal currentSelectionMaterial ()) $ filter ((\x -> x `elem` map Just highlightObjs) . ident) drawCommands
      processCustomCommands frameContext (filter (\x -> x.stage == ShowSelection && worldCullTest x) customDrawCommands)

    useRenderTarget shadowRenderTarget commandBuffer [ DepthStencil (ClearDepthStencilValue 1 0) ] swapchainImageIndex do
      processRenderPass frameContext
        ( Universal animatedShadowMaterial ()
        , Universal staticShadowMaterial ()
        , NullMat
        , NullMat
        , NullMat
        ) $ filter castsShadow drawCommands
      processCustomCommands frameContext (filter (\x -> x.stage == ShadowMap && shadowCullTest x) customDrawCommands)

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
        ) drawCommands
      processCustomCommands frameContext (filter (\x -> x.stage == World && worldCullTest x) customDrawCommands)

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
      processCustomCommands frameContext (filter (\x -> x.stage == Overlay && overlayCullTest x) customDrawCommands)
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
  bindMaterialIfNeeded frameContext (fst config)
  renderCommand frameContext (fst config) mesh ds (fromIntegral $ length unis)
  liftIO $ modifyIORef (snd config) (uniform:)

bindMaterialIfNeeded :: (MonadState UUID m, MonadIO m) => FrameContext -> BufferedUniformMaterial uniform -> m ()
bindMaterialIfNeeded fc BufferedUniformMaterial {..} = do
  curUUID <- get
  when (curUUID /= uuid material) do
    cmdBindMaterial fc material
    put curUUID

renderCommand
  :: (MonadIO m, DynamicMeshMonad m)
  => FrameContext
  -> BufferedUniformMaterial uniform
  -> MeshType
  -> Maybe PointedDescriptorSet
  -> Word32
  -> m ()
renderCommand FrameContext {..} BufferedUniformMaterial {..} mesh drawDS uniformIdx = do
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
processCustomCommands
  :: (MonadIO m, DynamicMeshMonad m)
  => FrameContext
  -> [CustomDrawCommand]
  -> m ()
processCustomCommands fc commands = do
  -- First, the opaque commands, sorted by material
  for_ grouped \group ->
    case headMay group of
      Nothing -> pure ()
      Just CustomDrawCommand { material } -> do
        cmdBindMaterial fc material.material
        let BufferDescriptorSet { dataBuffer } = resourceForFrame fc.swapchainImageIndex material.descriptor
            DataBuffer {..} = dataBuffer
        for_ (zip group [0..]) \(CustomDrawCommand {mesh, descriptorSet}, i) -> do
          renderCommand fc material mesh descriptorSet i
        liftIO $ withMappedMemory allocator allocation bracket \bufptr -> do
          for_ (zip group [0..]) \(CustomDrawCommand {pokeData}, i) -> do
              liftIO $ pokeData (plusPtr bufptr (material.uniformSize * i))

  -- Now the blended commands
  flip evalStateT (initialMaterialCounts, UUID.nil) do
    for_ blended \CustomDrawCommand {mesh, descriptorSet, material, pokeData} -> do
      (m,_) <- get
      let i = fromMaybe 0 (HashMap.lookup material.material.uuid m)
      zoom _2 $ bindMaterialIfNeeded fc material
      lift $ renderCommand fc material mesh descriptorSet (fromIntegral i)

      let BufferDescriptorSet { dataBuffer } = resourceForFrame fc.swapchainImageIndex material.descriptor
          DataBuffer {..} = dataBuffer
      liftIO $ withMappedMemory allocator allocation bracket \bufptr -> do
        pokeData (plusPtr bufptr (material.uniformSize * i))
      _1 %= HashMap.insert material.material.uuid (i + 1)
  where
  (blended, opaque) = partition doBlend commands
  grouped = groupBy (\CustomDrawCommand { material = m1 } CustomDrawCommand { material = m2 } -> m1.material.uuid == m2.material.uuid) opaque
  initialMaterialCounts :: HashMap.HashMap UUID Int = HashMap.fromList $ flip mapMaybe grouped \group -> case headMay group of
    Just CustomDrawCommand { material } -> Just (material.material.uuid, length group)
    Nothing -> Nothing
