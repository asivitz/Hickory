{-# LANGUAGE DataKinds, OverloadedLists, OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts, OverloadedRecordDot #-}

module Hickory.Vulkan.Forward.Renderer where

import Hickory.Vulkan.Forward.Types (Renderer (..), DrawCommand (..), StaticConstants (..), MeshType (..), AnimatedMesh (..), AnimatedConstants (..), Command, MSDFMesh (..), RenderSettings (..), StaticMesh (..), DrawType (..), addCommand, CommandMonad, runCommand, highlightObjs, Globals(..), WorldGlobals (..), WorldSettings (..), OverlayGlobals (..), RenderTargets (..), ShadowGlobals (ShadowGlobals), ShadowPushConsts (..), GBufferMaterialStack(..), DirectMaterial(..), DirectStage (..), MaterialConfig (..), GBufferPushConsts (..))
import Hickory.Vulkan.Vulkan ( mkAcquire)
import Acquire.Acquire (Acquire)
import Hickory.Vulkan.PostProcessing (withPostProcessMaterial)
import Linear (V4 (..), transpose, inv33, _m33, V2 (..), V3 (..), (!*!), inv44, (!*), _x, _y, _z, _w, (^/), distance, normalize, dot, cross, norm, Epsilon)
import Hickory.Vulkan.Monad (material, BufferedUniformMaterial (..), cmdDrawBufferedMesh, getMeshes, addMesh, askDynamicMesh, useDynamicMesh, DynamicMeshMonad, textMesh)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Hickory.Vulkan.Types (RenderConfig (..), DescriptorSpec (..), PointedDescriptorSet, buf, hasPerDrawDescriptorSet, Material(..), DeviceContext (..), VulkanResources (..), Swapchain, FrameContext (..), BufferedMesh (..), vertices, indices, DataBuffer (..), Mesh (..))
import qualified Hickory.Vulkan.Types as HVT
import Hickory.Vulkan.Text (withMSDFMaterial, MSDFMatConstants (..), TextRenderer, withOverlayMSDFMaterial, msdfVertShader, msdfFragShader)
import Hickory.Vulkan.Forward.GBuffer (withGBufferRenderConfig, withLineMaterial, withPointMaterial, staticGBufferVertShader, staticGBufferFragShader, animatedGBufferVertShader, animatedGBufferFragShader, unlitFragShader, staticUnlitVertShader, overlayVertShader, withGBufferFrameBuffer, withDepthViewableImage, staticGBufferShadowVertShader, animatedGBufferShadowVertShader)
import Hickory.Vulkan.Forward.ShadowPass (withShadowRenderConfig, staticVertShader, whiteFragShader, animatedVertShader, withShadowMap)
import Hickory.Vulkan.RenderPass (withSwapchainRenderConfig, useRenderConfig, withSwapchainFramebuffers)
import Hickory.Vulkan.Mesh (vsizeOf, attrLocation, numVerts)
import Vulkan (ClearValue (..), ClearColorValue (..), cmdDraw, ClearDepthStencilValue (..), bindings, withDescriptorSetLayout, BufferUsageFlagBits (..), Extent2D (..), DescriptorSetLayout, ImageLayout (..))
import Foreign (Storable, plusPtr, sizeOf, poke)
import Hickory.Vulkan.DescriptorSet (withDescriptorSet, BufferDescriptorSet (..), descriptorSetBindings, withDataBuffer, uploadBufferDescriptor, uploadBufferDescriptorArray, withBufferDescriptorSet)
import Control.Lens (view, (^.), (.~), (&), _1, _2, _3, _4, _5, has, (^?), over)
import Hickory.Vulkan.Framing (resourceForFrame, frameResource, withResourceForFrame, FramedResource)
import Hickory.Vulkan.Material (cmdBindMaterial, cmdPushMaterialConstants, cmdBindDrawDescriptorSet, PipelineOptions (..), withMaterial, pipelineDefaults, noBlend, defaultBlend)
import Data.List (partition, sortOn, mapAccumL)
import Data.Foldable (for_)
import Hickory.Vulkan.DynamicMesh (DynamicBufferedMesh(..), withDynamicBufferedMesh)
import qualified Data.Vector as V
import Vulkan.Zero (zero)
import Hickory.Vulkan.Forward.ObjectPicking (withObjectIDRenderConfig, ObjectIDConstants (..), withObjectHighlightMaterial, withObjectIDFrameBuffer)
import qualified Hickory.Vulkan.Forward.ObjectPicking as OP
import Linear.Matrix (M44)
import Hickory.Text.Types ( TextCommand(..) )
import Control.Monad (when, unless, void)
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
import Control.Arrow ((&&&))
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Sized as VS
import Hickory.Vulkan.Forward.ShaderDefinitions (maxShadowCascades)
import Hickory.Vulkan.Forward.Direct (withDirectRenderConfig, withDirectFrameBuffer, staticDirectVertShader, staticDirectFragShader)
import Hickory.Vulkan.Forward.Lights (withDirectionalLightMaterial, withLightingRenderConfig, withLightingFrameBuffer, withColorViewableImage)
import Hickory.Vulkan.Textures (transitionImageLayout)

withRendererMaterial
  :: forall a. Storable a
  => VulkanResources
  -> RenderConfig
  -> [HVT.Attribute]
  -> PipelineOptions
  -> B.ByteString
  -> B.ByteString
  -> [FramedResource PointedDescriptorSet]
  -> Maybe DescriptorSetLayout -- Per draw descriptor set
  -> Acquire (Material a)
withRendererMaterial vulkanResources renderConfig = withMaterial vulkanResources renderConfig

withGBufferMaterialStack
  :: forall uniform
  .  Storable uniform
  => VulkanResources
  -> RenderTargets
  -> FramedResource PointedDescriptorSet
  -> Int
  -> PipelineOptions
  -> [HVT.Attribute]
  -> Maybe DescriptorSetLayout -- Per draw descriptor set
  -> ByteString
  -> ByteString
  -> ByteString
  -> Acquire (MaterialConfig uniform)
withGBufferMaterialStack vulkanResources RenderTargets {..} globalDescriptorSet maxNumDraws pipelineOptions attributes perDrawLayout
  gbufferVertShader gbufferFragShader
  shadowVertShader
  = GBufferConfig <$> do
  uuid <- liftIO nextRandom
  descriptor <- frameResource $ withBufferDescriptorSet vulkanResources maxNumDraws
  let uniformSize = sizeOf (undefined :: uniform)
      materialSet = view #descriptorSet <$> descriptor
      materialSets =
        [ globalDescriptorSet
        , materialSet
        ]

  gbufferMaterial <- withRendererMaterial vulkanResources gbufferRenderConfig attributes pipelineOptions gbufferVertShader gbufferFragShader materialSets perDrawLayout
  shadowMaterial  <- withRendererMaterial vulkanResources shadowRenderConfig attributes pipelineOptions { depthClampEnable = True } shadowVertShader whiteFragShader materialSets perDrawLayout
  showSelectionMaterial <- withRendererMaterial vulkanResources objectIDRenderConfig attributes pipelineOptions { colorBlends = [noBlend]} gbufferVertShader OP.objectIDFragShader materialSets perDrawLayout
  pure GBufferMaterialStack {..}

withStaticGBufferMaterialConfig :: VulkanResources -> RenderTargets -> FramedResource PointedDescriptorSet -> Maybe DescriptorSetLayout -> Acquire (MaterialConfig StaticConstants)
withStaticGBufferMaterialConfig vulkanResources renderTargets globalPDS perDrawLayout =
  withGBufferMaterialStack vulkanResources renderTargets globalPDS standardMaxNumDraws (pipelineDefaults [noBlend, noBlend, noBlend]) [HVT.Position, HVT.Normal, HVT.TextureCoord] perDrawLayout staticGBufferVertShader staticGBufferFragShader staticGBufferShadowVertShader

withAnimatedGBufferMaterialConfig :: VulkanResources -> RenderTargets -> FramedResource PointedDescriptorSet -> Maybe DescriptorSetLayout -> Acquire (MaterialConfig AnimatedConstants)
withAnimatedGBufferMaterialConfig vulkanResources renderTargets globalPDS perDrawLayout =
  withGBufferMaterialStack vulkanResources renderTargets globalPDS standardMaxNumDraws (pipelineDefaults [noBlend, noBlend, noBlend]) [HVT.Position, HVT.Normal, HVT.TextureCoord, HVT.JointIndices, HVT.JointWeights] perDrawLayout animatedGBufferVertShader animatedGBufferFragShader animatedGBufferShadowVertShader

withDirectMaterialStack
  :: forall uniform
  .  Storable uniform
  => VulkanResources
  -> RenderTargets
  -> FramedResource PointedDescriptorSet
  -> Int
  -> PipelineOptions
  -> [HVT.Attribute]
  -> Maybe DescriptorSetLayout -- Per draw descriptor set
  -- -> DirectStage
  -> ByteString
  -> ByteString
  -> Acquire (MaterialConfig uniform)
withDirectMaterialStack vulkanResources RenderTargets {..} globalDescriptorSet maxNumDraws pipelineOptions attributes perDrawLayout
  vertShader fragShader
  = DirectConfig <$> do
  uuid <- liftIO nextRandom
  descriptor <- frameResource $ withBufferDescriptorSet vulkanResources maxNumDraws
  let uniformSize = sizeOf (undefined :: uniform)
      materialSet = view #descriptorSet <$> descriptor
      materialSets =
        [ globalDescriptorSet
        , materialSet
        ]

  directMaterial  <- withRendererMaterial vulkanResources directRenderConfig  attributes pipelineOptions vertShader fragShader materialSets perDrawLayout
  overlayMaterial <- withRendererMaterial vulkanResources swapchainRenderConfig  attributes pipelineOptions vertShader fragShader materialSets perDrawLayout
  pure DirectMaterial {..}

withStaticDirectMaterialConfig :: VulkanResources -> RenderTargets -> FramedResource PointedDescriptorSet -> Maybe DescriptorSetLayout -> Acquire (MaterialConfig StaticConstants)
withStaticDirectMaterialConfig vulkanResources renderTargets globalPDS perDrawLayout =
  withDirectMaterialStack vulkanResources renderTargets globalPDS standardMaxNumDraws (pipelineDefaults [defaultBlend]) [HVT.Position, HVT.TextureCoord] perDrawLayout staticDirectVertShader staticDirectFragShader

withMSDFMaterialConfig :: VulkanResources -> RenderTargets -> FramedResource PointedDescriptorSet -> Maybe DescriptorSetLayout -> Acquire (MaterialConfig MSDFMatConstants)
withMSDFMaterialConfig vulkanResources renderTargets globalPDS perDrawLayout =
  withDirectMaterialStack vulkanResources renderTargets globalPDS standardMaxNumDraws (pipelineDefaults [defaultBlend]) [HVT.Position, HVT.TextureCoord] perDrawLayout msdfVertShader msdfFragShader

standardMaxNumDraws :: Num a => a
standardMaxNumDraws = 2048

{-
withStandardStaticMaterial :: VulkanResources -> Renderer -> Acquire (AllStageMaterial StaticConstants)
withStandardStaticMaterial vulkanResources Renderer {..} = withAllStageMaterial vulkanResources renderTargets globalDescriptorSet standardMaxNumDraws
  pipelineDefaults [HVT.Position, HVT.Normal, HVT.TextureCoord] (Just imageSetLayout)
  staticLitVertShader staticLitFragShader
  staticVertShader whiteFragShader
  OP.staticObjectIDVertShader OP.staticObjectIDFragShader
  Nothing Nothing

withStandardStaticUnlitMaterial :: VulkanResources -> Renderer -> Acquire (AllStageMaterial StaticConstants)
withStandardStaticUnlitMaterial vulkanResources Renderer {..} = withAllStageMaterial vulkanResources renderTargets globalDescriptorSet standardMaxNumDraws
  pipelineDefaults [HVT.Position, HVT.TextureCoord] (Just imageSetLayout)
  staticUnlitVertShader unlitFragShader
  staticVertShader whiteFragShader
  OP.staticObjectIDVertShader OP.staticObjectIDFragShader
  (Just overlayVertShader) (Just unlitFragShader)

withStandardAnimatedMaterial :: VulkanResources -> Renderer -> Acquire (AllStageMaterial AnimatedConstants)
withStandardAnimatedMaterial vulkanResources Renderer {..} = withAllStageMaterial vulkanResources renderTargets globalDescriptorSet standardMaxNumDraws
  pipelineDefaults [HVT.Position, HVT.Normal, HVT.TextureCoord, HVT.JointIndices, HVT.JointWeights] (Just imageSetLayout)
  animatedLitVertShader animatedLitFragShader
  animatedVertShader whiteFragShader
  OP.animatedObjectIDVertShader OP.animatedObjectIDFragShader
  Nothing Nothing

withStandardMSDFMaterial :: VulkanResources -> Renderer -> Acquire (AllStageMaterial MSDFMatConstants)
withStandardMSDFMaterial vulkanResources Renderer {..} = withAllStageMaterial vulkanResources renderTargets globalDescriptorSet standardMaxNumDraws
  pipelineDefaults [HVT.Position, HVT.TextureCoord] (Just imageSetLayout)
  msdfVertShader msdfFragShader
  staticVertShader whiteFragShader
  OP.staticObjectIDVertShader OP.objectIDFragShader
  Nothing Nothing
  -}

withRenderer :: VulkanResources -> Swapchain -> Acquire Renderer
withRenderer vulkanResources@VulkanResources {deviceContext = DeviceContext{..}} swapchain = do
  shadowRenderConfig <- withShadowRenderConfig vulkanResources
  cascadedShadowMap <- frameResource $ withShadowMap vulkanResources shadowRenderConfig

  colorViewableImage    <- frameResource $ withColorViewableImage vulkanResources swapchain.extent
  depthViewableImage    <- frameResource $ withDepthViewableImage vulkanResources swapchain.extent

  gbufferRenderConfig   <- withGBufferRenderConfig vulkanResources swapchain
  gbufferRenderFrame    <- depthViewableImage & V.mapM (withGBufferFrameBuffer vulkanResources gbufferRenderConfig)
  lightingRenderConfig  <- withLightingRenderConfig vulkanResources swapchain
  lightingRenderFrame   <- colorViewableImage & V.mapM (withLightingFrameBuffer vulkanResources lightingRenderConfig)
  directRenderConfig    <- withDirectRenderConfig vulkanResources swapchain
  directRenderFrame     <- V.zip colorViewableImage depthViewableImage & V.mapM (uncurry $ withDirectFrameBuffer vulkanResources directRenderConfig)
  swapchainRenderConfig <- withSwapchainRenderConfig vulkanResources swapchain
  swapchainRenderFrame  <- withSwapchainFramebuffers vulkanResources swapchain swapchainRenderConfig
  objectIDRenderConfig  <- withObjectIDRenderConfig vulkanResources swapchain
  -- pickingRenderFrame    <- frameResource $ withObjectIDFrameBuffer vulkanResources objectIDRenderConfig
  currentSelectionRenderFrame <- frameResource $ withObjectIDFrameBuffer vulkanResources objectIDRenderConfig

  globalBuffer             <- frameResource $ withDataBuffer vulkanResources 1 BUFFER_USAGE_UNIFORM_BUFFER_BIT
  globalShadowPassBuffer   <- frameResource $ withDataBuffer vulkanResources 1 BUFFER_USAGE_UNIFORM_BUFFER_BIT
  globalWorldBuffer        <- frameResource $ withDataBuffer vulkanResources 1 BUFFER_USAGE_UNIFORM_BUFFER_BIT
  globalOverlayBuffer      <- frameResource $ withDataBuffer vulkanResources 1 BUFFER_USAGE_UNIFORM_BUFFER_BIT

  globalDescriptorSet <- for (V.zip5 globalBuffer globalWorldBuffer globalOverlayBuffer globalShadowPassBuffer (pure . snd <$> cascadedShadowMap))
    \(globalBuf, globalWorldBuf, globalOverlayBuf, globalShadowPassBuf, targetDescriptorSpecs) -> do
      withDescriptorSet vulkanResources $
        [ BufferDescriptor (buf globalBuf)
        , BufferDescriptor (buf globalWorldBuf)
        , BufferDescriptor (buf globalOverlayBuf)
        , BufferDescriptor (buf globalShadowPassBuf)
        ] ++ targetDescriptorSpecs

  -- For debugging
  shadowMapDescriptorSet <- for (concatMap snd . VS.toList . fst <$> cascadedShadowMap) $ withDescriptorSet vulkanResources

  imageSetLayout <- withDescriptorSetLayout device zero
    { bindings = V.fromList $ descriptorSetBindings [ImageDescriptor [error "Dummy image"]]
    } Nothing mkAcquire


  -- currentSelectionMaterial <- withObjectIDMaterial vulkanResources objectIDRenderConfig globalDescriptorSet
  -- staticShadowMaterial     <- withStaticShadowMaterial vulkanResources shadowRenderConfig globalDescriptorSet
  -- animatedShadowMaterial   <- withAnimatedShadowMaterial vulkanResources shadowRenderConfig globalDescriptorSet

  {-
  pickingMaterial          <- withObjectIDMaterial vulkanResources objectIDRenderConfig globalDescriptorSet


  staticUnlitWorldMaterial <- withStaticUnlitMaterial vulkanResources litRenderConfig globalDescriptorSet imageSetLayout

  msdfWorldMaterial        <- withMSDFMaterial vulkanResources litRenderConfig globalDescriptorSet imageSetLayout
  linesWorldMaterial       <- withLineMaterial vulkanResources litRenderConfig globalDescriptorSet
  pointsWorldMaterial      <- withPointMaterial vulkanResources litRenderConfig globalDescriptorSet

  staticOverlayMaterial    <- withOverlayMaterial vulkanResources swapchainRenderConfig globalDescriptorSet imageSetLayout
  msdfOverlayMaterial      <- withOverlayMSDFMaterial vulkanResources swapchainRenderConfig globalDescriptorSet imageSetLayout


  objHighlightDescriptorSet <- for (snd <$> currentSelectionRenderFrame) $ withDescriptorSet vulkanResources

  objHighlightMaterial <- withObjectHighlightMaterial vulkanResources litRenderConfig globalDescriptorSet objHighlightDescriptorSet
  -}
  sunMaterialDescriptorSet <- for (snd <$> gbufferRenderFrame) $ withDescriptorSet vulkanResources
  sunMaterial              <- withDirectionalLightMaterial vulkanResources lightingRenderConfig globalDescriptorSet sunMaterialDescriptorSet

  postMaterialDescriptorSet <- for (snd <$> directRenderFrame) $ withDescriptorSet vulkanResources
  postProcessMaterial <- withPostProcessMaterial vulkanResources swapchainRenderConfig globalDescriptorSet postMaterialDescriptorSet

  dynamicMesh <- frameResource $ withDynamicBufferedMesh vulkanResources 10000 -- For text, need 20 floats per non-whitespace character

  objectPickingImageBuffer <- withImageBuffer vulkanResources objectIDRenderConfig 2 (snd <$> gbufferRenderFrame)

  let renderTargets = RenderTargets {..}

  staticGBufferMaterialConfig   <- withStaticGBufferMaterialConfig vulkanResources renderTargets globalDescriptorSet (Just imageSetLayout)
  animatedGBufferMaterialConfig <- withAnimatedGBufferMaterialConfig vulkanResources renderTargets globalDescriptorSet (Just imageSetLayout)
  staticDirectMaterialConfig    <- withStaticDirectMaterialConfig vulkanResources renderTargets globalDescriptorSet (Just imageSetLayout)
  msdfMaterialConfig            <- withMSDFMaterialConfig vulkanResources renderTargets globalDescriptorSet (Just imageSetLayout)

  pure Renderer {..}

data RegisteredMaterial const extra
  = NullMat
  | Universal (BufferedUniformMaterial Word32 const) extra
  | LitAndUnlit (BufferedUniformMaterial Word32 const, extra) (BufferedUniformMaterial Word32 const, extra)

isPointWithinCameraFrustum :: Scalar -> Camera -> V3 Scalar -> Bool
isPointWithinCameraFrustum _ Camera {projection = Ortho {}} _ = True -- Ortho culling not yet implemented
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
isSphereWithinCameraFrustum _ Camera {projection = Ortho {}} = \_ _ -> True -- Ortho culling not yet implemented
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

boundingSphere :: DrawCommand -> (V3 Float, Float)
boundingSphere DrawCommand {..} = (center, norm (max' - center))
  where
  (min', max') = case mesh of
    Dynamic  m -> (transformV3 modelMat m.minPosition, transformV3 modelMat m.maxPosition)
    Buffered m -> (transformV3 modelMat m.minPosition, transformV3 modelMat m.maxPosition)
  center = glerp 0.5 min' max'

renderToRenderer :: (MonadIO m) => FrameContext -> Renderer -> RenderSettings -> Command () -> Command () -> m ()
renderToRenderer frameContext@FrameContext{..} Renderer {..} RenderSettings {..} litF overlayF = do
  useDynamicMesh (resourceForFrame swapchainImageIndex dynamicMesh) do
    let WorldSettings {..} = worldSettings
        RenderTargets {..} = renderTargets
        Extent2D w h = extent
        lightOrigin = V3 0 0 0
        lightUp = V3 0 0 1
        lightView = viewDirection lightOrigin lightDirection lightUp -- Trying to get the whole scene in view of the sun
        projMat = cameraProjMat (Size (fromIntegral w) (fromIntegral h)) camera
        viewMat = cameraViewMat camera
        viewProjMat = projMat !*! viewMat
        invViewMat = inv44 viewMat
        invProjMat = inv44 projMat
        nearPlane = cameraNear camera
        farPlane = cameraFar camera
        worldGlobals = WorldGlobals { camPos = cameraPos camera, multiSampleCount = fromIntegral multiSampleCount, ..}
        testSphereInCameraFrustum = isSphereWithinCameraFrustum (realToFrac w / realToFrac h) camera
        testSphereInShadowFrustum = sphereWithinCameraFrustumFromLightPerspective viewProjMat lightView shadowRenderConfig.extent lightOrigin lightDirection lightUp
        inWorldCull cdc      = not cdc.cull || uncurry testSphereInCameraFrustum (boundingSphere cdc)
        worldCullTest        = inWorldCull
        pickingCullTest cdc  = isJust cdc.hasIdent && (not cdc.cull || uncurry testSphereInCameraFrustum (boundingSphere cdc))
        shadowCullTest cdc   = cdc.doCastShadow && (not cdc.cull || uncurry testSphereInShadowFrustum (boundingSphere cdc))
        overlayCullTest _    = True
        showSelectionCullTest cdc = isJust cdc.hasIdent && (cdc.hasIdent `elem` fmap Just highlightObjs) && inWorldCull cdc


        near = fromMaybe 0 $ camera ^? #projection . #_Perspective . _2
        far = fromMaybe 0 $ camera ^? #projection . #_Perspective . _3
        clipRange = far - near

        minZ = near
        maxZ = near + clipRange

        range = maxZ - minZ
        ratio = maxZ / minZ

        cascadeSplitLambda = 0.95

        -- From https://github.com/SaschaWillems/Vulkan/blob/master/examples/shadowmappingcascade/shadowmappingcascade.cpp
        splitDepths = VS.generate \i ->
          let p = realToFrac $ (fromIntegral i + 1) / realToFrac maxShadowCascades
              lg = minZ * (ratio ** p)
              uniform = minZ + range * p
          in cascadeSplitLambda * (lg - uniform) + uniform
        lightViewProjs = VS.generate \(fromIntegral -> i) ->
          let dist = VS.unsafeIndex splitDepths i
              lastDist = if i == 0 then near else VS.unsafeIndex splitDepths (i-1)
              cam = camera & #projection . #_Perspective . _2 .~ lastDist
                           & #projection . #_Perspective . _3 .~ dist
              lightProj = lightProjection (cameraProjMat (Size (fromIntegral w) (fromIntegral h)) cam !*! viewMat) lightView shadowRenderConfig.extent
          in  lightProj !*! lightView
        shadowPassGlobals = ShadowGlobals lightViewProjs splitDepths

    withResourceForFrame swapchainImageIndex globalBuffer \buf ->
      uploadBufferDescriptor buf $ Globals frameNumber

    withResourceForFrame swapchainImageIndex globalWorldBuffer \buf ->
      uploadBufferDescriptor buf worldGlobals

    withResourceForFrame swapchainImageIndex globalShadowPassBuffer $
      flip uploadBufferDescriptor shadowPassGlobals

    withResourceForFrame swapchainImageIndex globalOverlayBuffer $
      flip uploadBufferDescriptor overlayGlobals

    let bucketOn on = HashMap.elems . HashMap.fromListWith (++) . map (on &&& pure)
        dcMaterialUUID DrawCommand {..} = case materialConfig of
          GBufferConfig mat -> mat.uuid
          DirectConfig mat -> mat.uuid

    let worldDrawCommands = runCommand litF
        overlayDrawCommands = runCommand overlayF
        isGBufferDrawCommand DrawCommand {..} = case materialConfig of
          GBufferConfig _ -> True
          _ -> False
        (gbufDrawCommands, directWorldDrawCommands) = partition isGBufferDrawCommand worldDrawCommands

        -- Direct commands, whether world or overlay, could be using the same material stacks.
        -- So we need to upload all the uniforms together. But also need to
        -- preserve their original orders b/c direct commands are often blended.
        -- So we'll tag them with unique ids that we can later use to look up the uniform ids.
        directWorldDrawCommandsTagged   = zip [0..] directWorldDrawCommands
        directOverlayDrawCommandsTagged = zip [length directWorldDrawCommands..] overlayDrawCommands
        allDirectCommandsTagged = directWorldDrawCommandsTagged ++ directOverlayDrawCommandsTagged
        allDirectCommandsGrouped = bucketOn (dcMaterialUUID . snd) allDirectCommandsTagged
        groupHead = fromMaybe (error "empty group") . headMay

    -- upload uniforms
    gbufDCsGroupedByMaterial :: [[(Word32, DrawCommand)]] <- for (bucketOn dcMaterialUUID gbufDrawCommands) \group ->
      case groupHead group of
        DrawCommand {materialConfig} -> case materialConfig of
          GBufferConfig material -> do
            let BufferDescriptorSet { dataBuffer } = resourceForFrame swapchainImageIndex material.descriptor
                DataBuffer {..} = dataBuffer
            liftIO $ withMappedMemory allocator allocation bracket \bufptr -> do
              for (zip group [0..]) \(dc@DrawCommand {pokeData}, uniIdx) -> do
                liftIO $ pokeData (plusPtr bufptr (material.uniformSize * uniIdx))
                pure (fromIntegral uniIdx, dc)
          _ -> error "Non gbuffer draw commands not supported here"

    directDCIdsToUniformIdx :: HashMap.HashMap Int Word32 <- HashMap.fromList . concat <$> for allDirectCommandsGrouped \group -> do
      case groupHead group of
        (_,DrawCommand {materialConfig}) -> case materialConfig of
          DirectConfig material -> do
            let BufferDescriptorSet { dataBuffer } = resourceForFrame swapchainImageIndex material.descriptor
                DataBuffer {..} = dataBuffer
            liftIO $ withMappedMemory allocator allocation bracket \bufptr -> do
              for (zip group [0..]) \((dcId, DrawCommand {pokeData}), uniIdx) -> do
                liftIO $ pokeData (plusPtr bufptr (material.uniformSize * uniIdx))
                pure (dcId, fromIntegral uniIdx)
          _ -> error "Non direct draw commands not supported here"

    let directWorldDrawCommandsWithUniformIdx   = map (over _1 (fromMaybe (error "Can't find uniform index") . (`HashMap.lookup` directDCIdsToUniformIdx))) directWorldDrawCommandsTagged
        directOverlayDrawCommandsWithUniformIdx = map (over _1 (fromMaybe (error "Can't find uniform index") . (`HashMap.lookup` directDCIdsToUniformIdx))) directOverlayDrawCommandsTagged

    -- Stage 1 Shadows
    for_ ([0..maxShadowCascades-1] :: [Word32]) \i ->
      useRenderConfig shadowRenderConfig commandBuffer [ DepthStencil (ClearDepthStencilValue 1 0) ] swapchainImageIndex (fst . (`VS.unsafeIndex` fromIntegral i) . fst <$> cascadedShadowMap) do
        processDrawCommandShadows frameContext (filter (shadowCullTest . snd) <$> gbufDCsGroupedByMaterial) i

    -- Stage 2 Current Selection
    {- TODO Show selection pass
    useRenderConfig objectIDRenderConfig commandBuffer [ DepthStencil (ClearDepthStencilValue 1 0), Color (Uint32 0 0 0 0) ] swapchainImageIndex (fst <$> currentSelectionRenderFrame) do
      processCustomCommandGroups frameContext ShowSelection (filter (showSelectionCullTest . snd) <$> allWorldGrouped) Nothing
    -}

    -- Stage 3 GBuffer
    let V4 r g b a = clearColor
    useRenderConfig gbufferRenderConfig commandBuffer
      [ Color (Float32 r g b a)
      , Color (Float32 1 1 1 1)
      , Color (Uint32 0 0 0 0)
      , DepthStencil (ClearDepthStencilValue 1 0)
      ] swapchainImageIndex (fst <$> gbufferRenderFrame) do
      processDrawCommandGBuffer frameContext (filter (worldCullTest . snd) <$> gbufDCsGroupedByMaterial)

    -- Stage 4 Decals (TODO)
    -- Stage 5 Lighting
    useRenderConfig lightingRenderConfig commandBuffer [Color (Float32 0 0 0 1)] swapchainImageIndex (fst <$> lightingRenderFrame) do
      -- Sun is a full screen light
      cmdBindMaterial frameContext sunMaterial
      liftIO do
        cmdPushMaterialConstants commandBuffer sunMaterial 0
        cmdDraw commandBuffer 3 1 0 0

    -- We use depth as a texture for lighting, but need it for z-testing in forward rendering
    transitionImageLayout (resourceForFrame swapchainImageIndex depthViewableImage).image IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL commandBuffer

    -- Stage 6 Forward
    useRenderConfig directRenderConfig commandBuffer [Color (Float32 0 0 0 1)] swapchainImageIndex (fst <$> directRenderFrame) do
      -- Layer in extra direct color commands
      processDirectUngrouped frameContext (filter (worldCullTest . snd) directWorldDrawCommandsWithUniformIdx) WorldDirect

      {- TODO Show selection
      case highlightObjs of
        (_:_) -> do
          cmdBindMaterial frameContext objHighlightMaterial
          liftIO $ cmdDraw commandBuffer 3 1 0 0
        _ -> pure ()
      -}

    -- Stage 7 Post + Overlay
    void $ useRenderConfig swapchainRenderConfig commandBuffer [] swapchainImageIndex (fst <$> swapchainRenderFrame) do
      -- Post processing
      cmdBindMaterial frameContext postProcessMaterial
      liftIO $ cmdPushMaterialConstants commandBuffer postProcessMaterial postSettings
      liftIO $ cmdDraw commandBuffer 3 1 0 0

      -- Extra direct commands over the top
      processDirectUngrouped frameContext (filter (overlayCullTest . snd) directOverlayDrawCommandsWithUniformIdx) OverlayDirect

    -- Make object picking texture available for reading
    liftIO $ copyDescriptorImageToBuffer commandBuffer (resourceForFrame swapchainImageIndex objectPickingImageBuffer)

bindMaterialIfNeeded :: (MonadState UUID m, MonadIO m) => FrameContext -> Material Word32 -> m ()
bindMaterialIfNeeded fc material = do
  curUUID <- get
  when (curUUID /= material.uuid) do
    cmdBindMaterial fc material
    put curUUID

renderCommand
  :: (MonadIO m, DynamicMeshMonad m, Storable a)
  => FrameContext
  -> Material a
  -> MeshType
  -> Word32
  -> Maybe PointedDescriptorSet
  -> a
  -> m ()
renderCommand FrameContext {..} material mesh instanceCount drawDS pushConsts = do
  cmdPushMaterialConstants commandBuffer material pushConsts
  when (hasPerDrawDescriptorSet material) do
    for_ drawDS $ cmdBindDrawDescriptorSet commandBuffer material
  case mesh of
    Buffered BufferedMesh {..} -> cmdDrawBufferedMesh commandBuffer material 0 meshOffsets vertexBuffer instanceCount numIndices numVertices 0 indexBuffer
    Dynamic dyn -> do
      meshes <- getMeshes
      addMesh dyn

      -- This is O(n)... Might want to cache this
      let vertexSizeThusFar = sum $ map (sum . map (vsizeOf . snd) . vertices) meshes
          indexSizeThusFar  = sum $ map (maybe 0 vsizeOf . indices) meshes
          numVertices = fromIntegral $ numVerts dyn
          numIndices = fromIntegral . SV.length <$> dyn.indices
          meshOffsets = snd $ mapAccumL (\s (a,vec) -> (s + vsizeOf vec, (a, s))) 0 (sortOn (attrLocation . fst) dyn.vertices)

      DynamicBufferedMesh { vertexBufferPair = (vertexBuffer,_), indexBufferPair = (indexBuffer,_) } <- askDynamicMesh
      cmdDrawBufferedMesh commandBuffer material vertexSizeThusFar meshOffsets vertexBuffer instanceCount numIndices numVertices (fromIntegral indexSizeThusFar) (Just indexBuffer)

uploadUniformsBuffer :: (MonadIO m, Storable a) => FrameContext -> BufferedUniformMaterial pushConst a -> [a] -> m ()
uploadUniformsBuffer FrameContext {..} BufferedUniformMaterial {..} uniforms = do
  let BufferDescriptorSet { dataBuffer } = resourceForFrame swapchainImageIndex descriptor

  uploadBufferDescriptorArray dataBuffer uniforms

drawText
  :: CommandMonad m
  => MaterialConfig MSDFMatConstants
  -> TextRenderer
  -> M44 Float
  -> V4 Float
  -> V4 Float
  -> Float
  -> TextCommand
  -> m ()
drawText materialConfig (font, fontTex, sdfPixelRange) mat color outlineColor outlineSize tc =
  addCommand $ DrawCommand
    { modelMat        = mat
    , mesh            = Dynamic (textMesh font tc)
    , instanceCount   = 1
    , materialConfig  = materialConfig
    , doCastShadow    = False
    , doBlend         = True
    , hasIdent        = Nothing
    , descriptorSet   = Just fontTex
    , cull = False
    , pokeData = flip poke $ MSDFMatConstants
        { modelMat      = mat
        , color         = color
        , outlineColor  = outlineColor
        , outlineSize   = outlineSize
        , sdfPixelRange = sdfPixelRange
        , tiling        = V2 1 1
        }
    }

pickObjectID :: FrameContext -> Renderer -> (Scalar,Scalar) -> IO Int
pickObjectID FrameContext {..} Renderer{..} = fmap fromIntegral . readPixel (resourceForFrame (swapchainImageIndex - 1) objectPickingImageBuffer)

processDrawCommandShadows
  :: (MonadIO m, DynamicMeshMonad m)
  => FrameContext
  -> [[(Word32, DrawCommand)]]
  -> Word32
  -> m ()
processDrawCommandShadows fc grouped shadowCascadeIndex = do
  for_ grouped \group ->
    case headMay group of
      Just (_, DrawCommand { materialConfig }) -> case materialConfig of
        GBufferConfig GBufferMaterialStack {..} -> do
          cmdBindMaterial fc shadowMaterial
          for_ group \(i, DrawCommand {mesh, descriptorSet, instanceCount}) -> do
            renderCommand fc shadowMaterial mesh instanceCount descriptorSet (ShadowPushConsts i shadowCascadeIndex)
        _ -> error "Only gbuffer rendering supported here"
      _ -> pure ()

processDrawCommandGBuffer
  :: (MonadIO m, DynamicMeshMonad m)
  => FrameContext
  -> [[(Word32, DrawCommand)]]
  -> m ()
processDrawCommandGBuffer fc grouped = do
  for_ grouped \group ->
    case headMay group of
      Just (_, DrawCommand { materialConfig }) -> case materialConfig of
        GBufferConfig GBufferMaterialStack {..} -> do
          cmdBindMaterial fc gbufferMaterial
          for_ group \(i, DrawCommand {mesh, descriptorSet, instanceCount, hasIdent}) -> do
            renderCommand fc gbufferMaterial mesh instanceCount descriptorSet (GBufferPushConsts i (fromIntegral $ fromMaybe 0 hasIdent))
        _ -> error "Only gbuffer rendering supported here"
      _ -> pure ()

processDirectUngrouped
  :: (MonadIO m, DynamicMeshMonad m)
  => FrameContext
  -> [(Word32, DrawCommand)]
  -> DirectStage
  -> m ()
processDirectUngrouped fc commands stage = do
  flip evalStateT UUID.nil do
    for_ commands \(i, DrawCommand {mesh, descriptorSet, materialConfig, instanceCount}) -> case materialConfig of
      DirectConfig material -> do
        let mat = if stage == WorldDirect then material.directMaterial else material.overlayMaterial
        bindMaterialIfNeeded fc mat
        renderCommand fc mat mesh instanceCount descriptorSet (fromIntegral i)
      _ -> error "Only direct rendering supported here"
