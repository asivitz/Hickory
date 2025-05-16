{-# LANGUAGE DataKinds, OverloadedLists, OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts, OverloadedRecordDot, OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies, DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hickory.Vulkan.Renderer.Renderer where

import Hickory.Vulkan.Renderer.Types (Renderer (..), DrawCommand (..), MeshType (..), Command, RenderSettings (..), addCommand, CommandMonad, runCommand, highlightObjs, Globals(..), WorldGlobals (..), WorldSettings (..), RenderTargets (..), ShadowGlobals (ShadowGlobals), GBufferMaterialStack(..), DirectMaterial(..), MaterialConfig (..), MaterialDescriptorSet (..), DrawBatch (..), DrawConfig (..), DecalMaterial (..), debugName, Features(..))
import Hickory.Vulkan.Vulkan ( mkAcquire, with2DImageView)
import Acquire (Acquire)
import Hickory.Vulkan.PostProcessing (withPostProcessMaterial)
import Linear (V4 (..), V2 (..), V3 (..), (!*!), inv44, (!*), _x, _y, _z, _w, (^/), distance, normalize, dot, cross, norm, Epsilon, (^*))
import Hickory.Vulkan.Monad (material, BufferedUniformMaterial (..), getMeshes, addMesh, askDynamicMesh, useDynamicMesh, DynamicMeshMonad, textMesh)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Hickory.Vulkan.Types (RenderConfig (..), DescriptorSpec (..), buf, hasPerDrawDescriptorSet, Material(..), DeviceContext (..), VulkanResources (..), Swapchain, FrameContext (..), BufferedMesh (..), vertices, indices, DataBuffer (..), Mesh (..), BufferedMeshMember (..), ViewableImage (..))
import qualified Hickory.Vulkan.Types as HVT
import Hickory.Vulkan.Text (MSDFMatConstants (..), TextRenderer)
import Hickory.Vulkan.Renderer.GBuffer (withGBufferRenderConfig, withDepthViewableImage, withAlbedoViewableImage, withNormalViewableImage, withObjIDViewableImage, withMaterialViewableImage)
import Hickory.Vulkan.Renderer.ShadowPass (withShadowRenderConfig, withShadowMap)
import Hickory.Vulkan.RenderPass (useRenderConfig, createFramebuffer, renderConfigRenderPass)
import Hickory.Vulkan.Mesh (vsizeOf, attrLocation, numVerts)
import Vulkan (ClearValue (..), ClearColorValue (..), cmdDraw, ClearDepthStencilValue (..), bindings, withDescriptorSetLayout, BufferUsageFlagBits (..), Extent2D (..), ImageLayout (..), cmdBindVertexBuffers, cmdBindIndexBuffer, IndexType (..), cmdDrawIndexed, ShaderStageFlagBits (..), cmdPushConstants, Format (..), Filter (..), SamplerAddressMode (..), SamplerMipmapMode (..), ImageAspectFlagBits (..), ImageViewType (..), Extent3D (..), ImageType (..), RenderingInfo(..), RenderingAttachmentInfo(..), cmdUseRendering, AttachmentLoadOp (..), AttachmentStoreOp (..), Rect2D (..), PipelineStageFlagBits (..), AccessFlagBits (..), SurfaceFormatKHR(..), ImageUsageFlagBits (..), SampleCountFlagBits (..), cmdBindPipeline, PipelineBindPoint (..), cmdBindDescriptorSets, MemoryBarrier(..), cmdPipelineBarrier, CommandBuffer)
import Foreign (Storable, plusPtr, sizeOf, poke, pokeArray, castPtr, with, (.|.), Bits (..))
import Hickory.Vulkan.DescriptorSet (withDescriptorSet, BufferDescriptorSet (..), descriptorSetBindings, withDataBuffer, uploadBufferDescriptor, uploadBufferDescriptorArray)
import Control.Lens (view, (^.), (.~), (&), _1, _2, _3, _4, (^?), over, toListOf, each, set)
import Hickory.Vulkan.Framing (resourceForFrame, frameResource, withResourceForFrame, zipFramedResources)
import Hickory.Vulkan.Material (cmdBindMaterial, cmdBindDrawDescriptorSet, cmdPushMaterialConstants)
import Data.List (sortOn, mapAccumL, foldl')
import Data.Foldable (for_)
import Hickory.Vulkan.DynamicMesh (DynamicBufferedMesh(..), withDynamicBufferedMesh)
import qualified Data.Vector as V
import Vulkan.Zero (zero)
import Hickory.Vulkan.Renderer.ObjectPicking (withCurrentSelectionRenderConfig, withObjectHighlightMaterial, withCurrentSelectionFrameBuffer)
import Linear.Matrix (M44)
import Hickory.Text.Types ( TextCommand(..) )
import Control.Monad (when)
import Hickory.Types (Size (..))
import Hickory.Camera (cameraViewMat, cameraPos, cameraProjMat, cameraNear, cameraFar, Camera (..), Projection (..))
import Hickory.Math.Matrix ( viewDirection )
import Data.Word (Word32)
import Data.UUID (UUID)
import Control.Monad.State.Class ( MonadState, put, get )
import qualified Data.UUID as UUID
import Data.Maybe (fromMaybe, mapMaybe)
import Hickory.Vulkan.RenderTarget (copyDescriptorImageToBuffer, withImageBuffer, readPixel)
import Hickory.Math (Scalar, orthographicProjection, transformV3, glerp)
import Data.Fixed (div')
import Data.Traversable (for)
import VulkanMemoryAllocator (withMappedMemory)
import Control.Exception (bracket)
import Safe (headMay)
import qualified Data.HashMap.Strict as HashMap
import Control.Arrow ((&&&))
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Sized as VS
import qualified Data.Vector.Storable.Sized as VSS
import Hickory.Vulkan.Renderer.ShaderDefinitions (maxShadowCascades, cascadeOverlapThreshold, MaxShadowCascadesNat, MaxSSAOKernelSizeNat, maxSSAOKernelSize)
import Hickory.Vulkan.Renderer.Direct (withDirectRenderConfig, withOverlayRenderConfig)
import Hickory.Vulkan.Renderer.Lights (withDirectionalLightMaterial, withLightingRenderConfig, withLightingFrameBuffer, withColorViewableImage)
import Hickory.Vulkan.Textures (withImageFromArray, withImageSampler, imageBarrier, withIntermediateImage)
import Hickory.Vulkan.Renderer.Stats (Stats (..))
import Data.Functor ((<&>))
import Data.Traversable.Compat (mapAccumM)
import Data.Text (Text)
import Data.Finite (getFinite)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Text.Printf (printf)
import Data.IORef (readIORef, newIORef, modifyIORef', writeIORef)
import Hickory.Vulkan.Renderer.SSAO (withSSAOMaterial, withSSAORenderConfig, withSSAOViewableImage)
import Control.Monad.Random (randomRIO)
import Hickory.Vulkan.Renderer.Decals (withDecalRenderConfig)
import Data.Bool (bool)
import Hickory.Vulkan.StockTexture (withWhiteImageDescriptor, withWhiteCubeImageDescriptor)
import Hickory.Vulkan.LUT (withBaseLUT)
import Hickory.Vulkan.Renderer.StockMaterials (withStaticGBufferMaterialConfig, withAnimatedGBufferMaterialConfig, withStaticDirectMaterialConfig, withMSDFMaterialConfig, withLineDirectMaterialConfig, withPointDirectMaterialConfig, withDecalMaterialConfig)
import Hickory.Vulkan.Renderer.Blur (BlurConstants (..), withBlurMaterial, withDepthOfFieldMaterial, DepthOfFieldConstants (..))

withRenderer :: VulkanResources -> Swapchain -> Acquire Renderer
withRenderer vulkanResources@VulkanResources {deviceContext = DeviceContext{..}} swapchain = do
  linearSampler <- withImageSampler vulkanResources FILTER_LINEAR SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE SAMPLER_MIPMAP_MODE_LINEAR
  nearestSampler <- withImageSampler vulkanResources FILTER_NEAREST SAMPLER_ADDRESS_MODE_REPEAT SAMPLER_MIPMAP_MODE_NEAREST
  shadowRenderConfig <- withShadowRenderConfig vulkanResources
  cascadedShadowMap <- frameResource $ withShadowMap vulkanResources shadowRenderConfig

  let packAttachments = fmap V.fromList . zipFramedResources . fmap (fmap (.imageView))

  colorViewableImage    <- frameResource $ withColorViewableImage vulkanResources swapchain.extent
  depthViewableImage    <- frameResource $ withDepthViewableImage vulkanResources swapchain.extent
  albedoViewableImage   <- frameResource $ withAlbedoViewableImage vulkanResources swapchain.extent
  normalViewableImage   <- frameResource $ withNormalViewableImage vulkanResources swapchain.extent
  materialViewableImage <- frameResource $ withMaterialViewableImage vulkanResources swapchain.extent
  objIDViewableImage    <- frameResource $ withObjIDViewableImage vulkanResources swapchain.extent

  ssaoViewableImage     <- frameResource $ withSSAOViewableImage vulkanResources swapchain.extent

  let gbufferFloatDesc = zipFramedResources [albedoViewableImage, normalViewableImage, materialViewableImage, depthViewableImage]
                <&> ImageDescriptor . fmap (,linearSampler)
      gbufferUIntDesc = zipFramedResources [objIDViewableImage]
                <&> ImageDescriptor . fmap (,nearestSampler)
      decalDesc = zipFramedResources [ objIDViewableImage <&> \i -> ImageDescriptor [(i, nearestSampler)]
                                     , depthViewableImage <&> \i -> ImageDescriptor [(i, linearSampler)]
                                     ]

  gbufferRenderConfig   <- withGBufferRenderConfig vulkanResources swapchain
  gbufferRenderFrame    <- for (packAttachments [albedoViewableImage, normalViewableImage, materialViewableImage, objIDViewableImage, depthViewableImage]) $
    createFramebuffer vulkanResources.deviceContext.device (renderConfigRenderPass gbufferRenderConfig) gbufferRenderConfig.extent
  for_ gbufferRenderFrame $ \fb -> debugName vulkanResources fb "GBufferFrameBuffer"

  decalRenderConfig     <- withDecalRenderConfig vulkanResources swapchain
  decalRenderFrame      <- for (packAttachments [albedoViewableImage, normalViewableImage, materialViewableImage]) $
    createFramebuffer vulkanResources.deviceContext.device (renderConfigRenderPass decalRenderConfig) decalRenderConfig.extent
  for_ decalRenderFrame $ \fb -> debugName vulkanResources fb "DecalFrameBuffer"

  ssaoRenderConfig      <- withSSAORenderConfig vulkanResources swapchain
  ssaoRenderFrame       <- ssaoViewableImage & V.mapM (withLightingFrameBuffer vulkanResources ssaoRenderConfig)
  lightingRenderConfig  <- withLightingRenderConfig vulkanResources swapchain
  lightingRenderFrame   <- colorViewableImage & V.mapM (withLightingFrameBuffer vulkanResources lightingRenderConfig)
  directRenderConfig    <- withDirectRenderConfig vulkanResources swapchain
  overlayRenderConfig   <- withOverlayRenderConfig vulkanResources swapchain
  currentSelectionRenderConfig  <- withCurrentSelectionRenderConfig vulkanResources swapchain
  currentSelectionRenderFrame <- frameResource $ withCurrentSelectionFrameBuffer vulkanResources currentSelectionRenderConfig

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

  for_ globalDescriptorSet \pds ->
    debugName vulkanResources pds.descriptorSet "GlobalDescSet"

  -- For debugging
  shadowMapDescriptorSet <- for (map snd . VS.toList . fst <$> cascadedShadowMap) $ withDescriptorSet vulkanResources
  for_ shadowMapDescriptorSet \pds ->
    debugName vulkanResources pds.descriptorSet "ShadowMapDescSet"

  singleImageSetLayout <- withDescriptorSetLayout device zero
    { bindings = V.fromList $ descriptorSetBindings [ImageDescriptor [error "Dummy image"]]
    } Nothing mkAcquire

  uberImageSetLayout <- withDescriptorSetLayout device zero
    { bindings = V.fromList $ descriptorSetBindings [ImageDescriptor [error "Dummy image"], ImageDescriptor [error "Dummy image"]]
    } Nothing mkAcquire


  -- currentSelectionMaterial <- withObjectIDMaterial vulkanResources objectIDRenderConfig globalDescriptorSet
  -- staticShadowMaterial     <- withStaticShadowMaterial vulkanResources shadowRenderConfig globalDescriptorSet
  -- animatedShadowMaterial   <- withAnimatedShadowMaterial vulkanResources shadowRenderConfig globalDescriptorSet

  objHighlightDescriptorSet <- for (pure . snd <$> currentSelectionRenderFrame) $ withDescriptorSet vulkanResources
  for_ objHighlightDescriptorSet \pds ->
    debugName vulkanResources pds.descriptorSet "ObjHighlightDescSet"
  objHighlightMaterial <- withObjectHighlightMaterial vulkanResources overlayRenderConfig globalDescriptorSet objHighlightDescriptorSet

  -- SSAO
  kernel :: VSS.Vector MaxSSAOKernelSizeNat (V3 Scalar) <- VSS.generateM \(getFinite -> i) -> do
    v <- fmap normalize $
         V3 <$> randomRIO (-1,1)
            <*> randomRIO (-1,1)
            <*> randomRIO (0,1)
    let scaleFac = realToFrac i / realToFrac maxSSAOKernelSize
        scale = glerp (scaleFac * scaleFac) 0.1 1
    pure $ v ^* scale

  kernelBuffer :: DataBuffer (VSS.Vector MaxSSAOKernelSizeNat (V3 Scalar)) <- withDataBuffer vulkanResources 1 BUFFER_USAGE_UNIFORM_BUFFER_BIT

  liftIO $ withMappedMemory kernelBuffer.allocator kernelBuffer.allocation bracket \bufptr -> poke (castPtr bufptr) kernel

  let noiseDim = 4
      noiseFormat = FORMAT_R32G32B32A32_SFLOAT
  noiseValues :: SV.Vector (V4 Scalar) <- SV.generateM (fromIntegral $ noiseDim * noiseDim) \_ ->
    normalize <$> (V4 <$> randomRIO (-1,1) <*> randomRIO (-1,1) <*> pure 0 <*> pure 0)

  (noiseImage,_) <- withImageFromArray vulkanResources (Extent3D noiseDim noiseDim 1) IMAGE_TYPE_2D noiseFormat False 1 zeroBits noiseValues
  imageView <- with2DImageView vulkanResources.deviceContext noiseFormat IMAGE_ASPECT_COLOR_BIT noiseImage IMAGE_VIEW_TYPE_2D 0 1

  ssaoMaterialDescriptorSet <- for gbufferFloatDesc $ \gbufFloatDesc -> withDescriptorSet vulkanResources
    [gbufFloatDesc, BufferDescriptor kernelBuffer.buf, ImageDescriptor [(ViewableImage noiseImage imageView noiseFormat,nearestSampler)]]
  for_ ssaoMaterialDescriptorSet \pds ->
    debugName vulkanResources pds.descriptorSet "SSAOMatDescSet"
  ssaoMaterial              <- withSSAOMaterial vulkanResources ssaoRenderConfig globalDescriptorSet ssaoMaterialDescriptorSet

  sunMaterialDescriptorSet <- for (V.zipWith (\a b -> [a,b]) gbufferFloatDesc (snd <$> ssaoRenderFrame)) $ withDescriptorSet vulkanResources
  for_ sunMaterialDescriptorSet \pds ->
    debugName vulkanResources pds.descriptorSet "SunMatDescSet"
  sunMaterial              <- withDirectionalLightMaterial vulkanResources lightingRenderConfig globalDescriptorSet sunMaterialDescriptorSet

  postMaterialDescriptorSet <- V.zip colorViewableImage depthViewableImage & V.mapM \(color, depth) -> do
    let descriptorSpec = ImageDescriptor
          [ (color,linearSampler)
          , (depth,linearSampler)
          ]
    withDescriptorSet vulkanResources [descriptorSpec]

  postImage <- frameResource do
    let format = swapchain.imageFormat.format
    raw  <- withIntermediateImage vulkanResources format (IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. IMAGE_USAGE_INPUT_ATTACHMENT_BIT .|. IMAGE_USAGE_SAMPLED_BIT) swapchain.extent SAMPLE_COUNT_1_BIT
    iv <- with2DImageView vulkanResources.deviceContext format IMAGE_ASPECT_COLOR_BIT raw IMAGE_VIEW_TYPE_2D 0 1
    debugName vulkanResources raw "PostImage"
    debugName vulkanResources iv "PostImageView"
    pure $ ViewableImage raw iv format

  for_ postMaterialDescriptorSet \pds ->
    debugName vulkanResources pds.descriptorSet "PostMatDescSet"
  postProcessMaterial <- withPostProcessMaterial vulkanResources overlayRenderConfig globalDescriptorSet postMaterialDescriptorSet

  dynamicMesh <- frameResource $ withDynamicBufferedMesh vulkanResources 10000 -- For text, need 20 floats per non-whitespace character

  objectPickingImageBuffer <- withImageBuffer vulkanResources swapchain.extent 0 gbufferUIntDesc

  {- Blur -}
  let blurFormat = swapchain.imageFormat.format
      blurExtent = swapchain.extent -- Extent2D (swapchain.extent.width `div` 2) (swapchain.extent.height `div` 2)
  blurDescriptorSet <- for postImage \i -> withDescriptorSet vulkanResources [ImageDescriptor [(i,linearSampler)]]

  blurMaterial <- withBlurMaterial vulkanResources blurFormat blurExtent blurDescriptorSet
  blurImage <- frameResource do
    raw <- withIntermediateImage vulkanResources blurFormat (IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. IMAGE_USAGE_INPUT_ATTACHMENT_BIT .|. IMAGE_USAGE_SAMPLED_BIT)
      blurExtent SAMPLE_COUNT_1_BIT
    iv <- with2DImageView vulkanResources.deviceContext blurFormat IMAGE_ASPECT_COLOR_BIT raw IMAGE_VIEW_TYPE_2D 0 1
    debugName vulkanResources raw "BlurImage"
    debugName vulkanResources iv "BlurImageView"
    pure $ ViewableImage raw iv blurFormat

  {- Depth of Field -}
  depthOfFieldDescriptorSet <- for (V.zip3 postImage blurImage depthViewableImage) \(p,b,d) ->
    withDescriptorSet vulkanResources [ ImageDescriptor [(p,linearSampler)]
                                      , ImageDescriptor [(b,linearSampler)]
                                      , ImageDescriptor [(d,linearSampler)]]
  depthOfFieldMaterial <- withDepthOfFieldMaterial vulkanResources swapchain.imageFormat.format swapchain.extent globalDescriptorSet depthOfFieldDescriptorSet

  let renderTargets = RenderTargets {..}

  staticGBufferMaterialConfig   <- withStaticGBufferMaterialConfig vulkanResources renderTargets globalDescriptorSet (Just uberImageSetLayout)
  (animatedGBufferMaterialConfig, skinBuffer) <- withAnimatedGBufferMaterialConfig vulkanResources renderTargets globalDescriptorSet (Just uberImageSetLayout)
  staticDirectMaterialConfig    <- withStaticDirectMaterialConfig vulkanResources renderTargets globalDescriptorSet (Just singleImageSetLayout)
  msdfMaterialConfig            <- withMSDFMaterialConfig vulkanResources renderTargets globalDescriptorSet (Just singleImageSetLayout)
  decalMaterialConfig           <- withDecalMaterialConfig vulkanResources renderTargets globalDescriptorSet (Just singleImageSetLayout)

  lineDirectMaterialConfig <- withLineDirectMaterialConfig vulkanResources renderTargets globalDescriptorSet
  pointDirectMaterialConfig <- withPointDirectMaterialConfig vulkanResources renderTargets globalDescriptorSet

  defaultEnvMapDescriptorSet <- do
    ds1 <- withWhiteCubeImageDescriptor vulkanResources
    ds2 <- withWhiteCubeImageDescriptor vulkanResources
    withDescriptorSet vulkanResources [ds1, ds2]

  defaultLutDescriptorSet <- do
    ds <- withBaseLUT vulkanResources
    withDescriptorSet vulkanResources [ds]

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

frustumSphereIntersection :: M44 Scalar -> V3 Scalar -> Scalar -> Bool
frustumSphereIntersection viewProjMat center radius =
  not $ any (\plane -> center `dot` plane.xyz + plane.w < -radius) planes
  where
  planes = normalizePlane <$> [left, right, bottom, top, near, far] :: [V4 Scalar]
  V4 r1 r2 r3 r4 = viewProjMat
  left   = r4 + r1
  right  = r4 - r1
  bottom = r4 + r2
  top    = r4 - r2
  near   = r4 + r3
  far    = r4 - r3
  normalizePlane (V4 x y z w) = let len = norm (V3 x y z) in V4 (x/len) (y/len) (z/len) (w/len)

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

data Stages a = Stages
  { gbuf     :: [a]
  , showSel  :: [a]
  , cascades :: VS.Vector MaxShadowCascadesNat [a]
  , direct   :: [a]
  , overlay  :: [a]
  , decals   :: [a]
  } deriving Generic

data SimpleCriteria = Equal | NonEqual
  deriving Generic
  deriving anyclass Hashable

instance Eq SimpleCriteria where
  Equal == Equal = True
  NonEqual == _ = False
  _ == NonEqual = False

emptyStages :: Stages a
emptyStages = Stages [] [] (VS.replicate @MaxShadowCascadesNat []) [] [] []

mapStagesLists :: ([a] -> [b]) -> Stages a -> Stages b
mapStagesLists f stages = Stages {..}
  where
  gbuf     = f stages.gbuf
  showSel  = f stages.showSel
  cascades = VS.map f stages.cascades
  direct   = f stages.direct
  overlay  = f stages.overlay
  decals   = f stages.decals

concatStages :: Stages a -> [a]
concatStages Stages {..} = concat (gbuf : showSel : direct : overlay : decals : VS.toList cascades)

-- f takes the current offset and the item, and returns the amount to advance and the new item
mapAccumOffset :: (Traversable t, Num a1) => (a1 -> a2 -> (a1, b)) -> t a2 -> (a1, t b)
mapAccumOffset f = mapAccumL g 0
  where
  g s a = let (off, r) = f s a in (s + off, r)

mapStagesAccumOffset :: Num s => (s -> a -> (s,b)) -> Stages a -> Stages b
mapStagesAccumOffset f stages = Stages {..}
  where
  (gbufLen, gbuf) = stages.gbuf & mapAccumOffset f
  (showSelLen, showSel) = stages.showSel & mapAccumOffset (\s a -> f (s + gbufLen) a)
  (directLen, direct) = stages.direct & mapAccumOffset (\s a -> f (s + gbufLen + showSelLen) a)
  (overlayLen, overlay) = stages.overlay & mapAccumOffset (\s a -> f (s + gbufLen + showSelLen + directLen) a)
  (decalsLen, decals) = stages.decals & mapAccumOffset (\s a -> f (s + gbufLen + showSelLen + directLen + overlayLen) a)
  cascades = fromMaybe (error "Lengths don't match") . VS.fromListN @MaxShadowCascadesNat . reverse . snd $ (\g -> VS.foldl' g (0, []) stages.cascades)
    \(offset,items) cascade -> let (off, newCascade) = mapAccumOffset (\s a -> f (s + offset + gbufLen + showSelLen + directLen + overlayLen + decalsLen) a) cascade
                               in (offset + off, newCascade : items)

transposeStages :: [Stages a] -> Stages [a]
transposeStages as = Stages {..}
  where
  gbuf = (.gbuf) <$> as
  showSel = (.showSel) <$> as
  allCascades = (.cascades) <$> as
  cascades = VS.generate @MaxShadowCascadesNat \i ->
    allCascades <&> (`VS.index` i)
  direct = (.direct) <$> as
  overlay = (.overlay) <$> as
  decals = (.decals) <$> as

unitTest :: [Int]
unitTest = concatStages $ stgs & mapStagesAccumOffset \s i -> (1, s+i)
  where
  stgs = Stages { gbuf = [1,2,3], showSel = [4,5,6], cascades = fromMaybe (error "") $ VS.fromListN @MaxShadowCascadesNat [[7],[8],[9]], direct = [10,11,12], overlay = [13,14,15], decals = [16,17] }

filterStages :: (a -> Bool) -> Stages a -> Stages a
filterStages f stages = Stages {..}
  where
  gbuf = filter f stages.gbuf
  showSel = filter f stages.showSel
  cascades = VS.map (filter f) stages.cascades
  direct = filter f stages.direct
  overlay = filter f stages.overlay
  decals = filter f stages.decals

renderToRenderer :: (MonadIO m) => FrameContext -> Renderer -> RenderSettings -> Command () -> Command () -> m Stats
renderToRenderer frameContext@FrameContext {..} Renderer {..} RenderSettings {..} litF overlayF = do
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
        gbufferSize = realToFrac <$> V2 w h

        diffuseMask  = bool 0 1 features.diffuse
        specularMask = bool 0 1 features.specular
        ssaoMask     = bool 0 1 features.ssao
        shadowsMask  = bool 0 1 features.shadows

        padding3 = 0
        padding4 = 0
        worldGlobals = WorldGlobals { camPos = cameraPos camera, multiSampleCount = fromIntegral multiSampleCount, ..}
        testSphereInCameraFrustum = frustumSphereIntersection viewProjMat

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
              cam = camera & #projection . #_Perspective . _2 .~ lastDist - (if i == 0 then 0 else cascadeOverlapThreshold)
                           & #projection . #_Perspective . _3 .~ dist
              lightProj = lightProjection (cameraProjMat (Size (fromIntegral w) (fromIntegral h)) cam !*! viewMat) lightView shadowRenderConfig.extent
          in  lightProj !*! lightView
        shadowPassGlobals = ShadowGlobals lightViewProjs splitDepths shadowBiasSlope

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
          DecalConfig mat -> mat.uuid

    let worldDrawCommands = runCommand litF
        overlayDrawCommands = runCommand overlayF
        -- Tag commands with an ordering
        -- An ordering of '0' is reserved for opaque commands, which we'll uncover later
        allDrawCommands = zip [1..] $ ((False,) <$> worldDrawCommands) ++ ((True,) <$> overlayDrawCommands)
        -- isGBufferDrawCommand DrawCommand {..} = case materialConfig of
        --   GBufferConfig _ -> True
        --   _ -> False
        -- (gbufDrawCommands, directWorldDrawCommands) = partition isGBufferDrawCommand worldDrawCommands

        -- Direct commands, whether world or overlay, could be using the same material stacks.
        -- So we need to upload all the uniforms together. But also need to
        -- preserve their original orders b/c direct commands are often blended.
        -- So we'll tag them with unique ids that we can later use to look up the uniform ids.
        -- directWorldDrawCommandsTagged   = zip [0..] directWorldDrawCommands
        -- directOverlayDrawCommandsTagged = zip [length directWorldDrawCommands..] overlayDrawCommands
        -- allDirectCommandsTagged = directWorldDrawCommandsTagged ++ directOverlayDrawCommandsTagged
        -- allDirectCommandsGrouped = bucketOn (dcMaterialUUID . snd) allDirectCommandsTagged
        groupHead = fromMaybe (error "empty group") . headMay

    -- upload uniforms
    -- For each DC we have the starting uniform index
    gbufDCsGroupedByMaterial :: [[(Word32, Int, Bool, DrawCommand)]] <- for (bucketOn (dcMaterialUUID . snd . snd) allDrawCommands) \group ->
      case groupHead group of
        (_, (_, DrawCommand {materialConfig})) ->
          let (materialDS, uniformSize) = case materialConfig of
                GBufferConfig material -> (material.descriptor, material.uniformSize)
                DirectConfig material -> (material.descriptor, material.uniformSize)
                DecalConfig material -> (material.descriptor, material.uniformSize)
          in do
            let MaterialDescriptorSet { uniformBuffer, idBuffer } = resourceForFrame swapchainImageIndex materialDS
            liftIO $ withMappedMemory uniformBuffer.allocator uniformBuffer.allocation bracket \uniformbufptr ->
                     withMappedMemory idBuffer.allocator idBuffer.allocation bracket \idbufptr -> do
                snd <$> (\f -> mapAccumM f 0 group) \startIdx (ordering, (isOverlay, dc@DrawCommand {pokeData, instances})) -> do
                  let objIds = concatMap (map fst . snd) instances
                  liftIO $ pokeData startIdx (plusPtr uniformbufptr (uniformSize * fromIntegral startIdx))
                  liftIO $ pokeArray (plusPtr idbufptr (sizeOf (undefined :: Word32) * fromIntegral startIdx)) objIds
                  pure (startIdx + fromIntegral (length objIds), (startIdx, ordering, isOverlay, dc))

    -- We expand the starting index for each DC into a list of non-culled indices
    -- Ex. A DC has 10 instances, starting at uniform index 17, but only 4 are left after culling. So we give indices 18, 19, 22, 24
    -- Of course we repeat this process for the view frustum as well as each cascade
    -- The front of the list will be the view frustum, followed by the cascades. We keep them together for the next processing step
    let cascadeTests = flip VS.map lightViewProjs $ uncurry . frustumSphereIntersection
        -- For each material, for each stage, we have a batch of instances to draw
        batches :: [Stages (DrawConfig, Text, Int, [Word32])]
        batches = map (filterStages (\(_,_,_,is) -> not $ null is))
                $ gbufDCsGroupedByMaterial <&> \as -> (\f -> foldl' f emptyStages as) \stgs (startUniformIndex, ordering, isOverlay, DrawCommand {..}) ->
          let boundingSpheres = instances <&> \(meshMemberName, idsAndTransforms) ->
                (meshMemberName, idsAndTransforms <&> \(_,t) -> meshBoundingSphere t meshMemberName mesh)
          in case materialConfig of
            DecalConfig stack ->
              let gbuf = stgs.gbuf
                  showSel = stgs.showSel
                  cascades = stgs.cascades
                  overlay = stgs.overlay
                  direct = stgs.direct
                  new = snd $ instances & mapAccumOffset \offset (meshMemberName, idsAndTransforms) ->
                    ( fromIntegral (length idsAndTransforms)
                    , ( DrawConfig { mesh
                                   , perDrawDescriptorSet = descriptorSet
                                   , material = stack.material
                                   , materialDescriptor = stack.descriptor
                                   }
                      , meshMemberName
                      , ordering
                      , flip mapMaybe (zip idsAndTransforms [0..]) \((_objId, _), i) ->
                          Just (offset + startUniformIndex + i))
                    )
                  decals = new ++ stgs.decals
              in Stages {..}
            DirectConfig stack ->
              let gbuf = stgs.gbuf
                  showSel = stgs.showSel
                  cascades = stgs.cascades
                  decals = stgs.decals
                  material = if isOverlay then stack.overlayMaterial else stack.directMaterial
                  new = snd $ instances & mapAccumOffset \offset (meshMemberName, idsAndTransforms) ->
                    ( fromIntegral (length idsAndTransforms)
                    , ( DrawConfig { mesh
                                   , perDrawDescriptorSet = descriptorSet
                                   , material = material
                                   , materialDescriptor = stack.descriptor
                                   }
                      , meshMemberName
                      , ordering
                      , flip mapMaybe (zip idsAndTransforms [0..]) \((_objId, _), i) ->
                          Just (offset + startUniformIndex + i))
                    )
                  direct = if isOverlay then stgs.direct else new ++ stgs.direct
                  overlay = if isOverlay then new ++ stgs.overlay else stgs.overlay
              in Stages {..}
            GBufferConfig gbufferMatStack ->
              let newGBuf  = snd $ boundingSpheres & mapAccumOffset \offset (meshMemberName, bss) ->
                              ( fromIntegral (length bss)
                              , ( DrawConfig { mesh
                                            , perDrawDescriptorSet = descriptorSet
                                            , material = gbufferMatStack.gbufferMaterial
                                            , materialDescriptor = gbufferMatStack.descriptor
                                            }
                                , meshMemberName
                                , 0
                                , flip mapMaybe (zip bss [0..]) \(bs, i) ->
                                    if not cull || uncurry testSphereInCameraFrustum bs then Just (offset + startUniformIndex + i) else Nothing)
                              )
                  newShowSel = snd $ instances & mapAccumOffset \offset (meshMemberName, idsAndTransforms) ->
                    ( fromIntegral (length idsAndTransforms)
                    , ( DrawConfig { mesh
                                  , perDrawDescriptorSet = descriptorSet
                                  , material = gbufferMatStack.showSelectionMaterial
                                  , materialDescriptor = gbufferMatStack.descriptor
                                  }
                      , meshMemberName
                      , 0
                      , flip mapMaybe (zip idsAndTransforms [0..]) \((objId, _), i) ->
                          if objId `elem` highlightObjs then Just (offset + startUniformIndex + i) else Nothing)
                    )
                  newCascades = cascadeTests & VS.map \test ->
                    snd $ boundingSpheres & mapAccumOffset \offset (meshMemberName, bss) ->
                      ( fromIntegral (length bss)
                      , ( DrawConfig { mesh
                                    , perDrawDescriptorSet = descriptorSet
                                    , material = gbufferMatStack.shadowMaterial
                                    , materialDescriptor = gbufferMatStack.descriptor
                                    }
                        , meshMemberName
                        , 0
                        , flip mapMaybe (zip bss [0..]) \(bs, i) ->
                            if test bs then Just (offset + startUniformIndex + i) else Nothing)
                      )
                  gbuf    = newGBuf ++ stgs.gbuf
                  showSel = newShowSel ++ stgs.showSel
                  cascades = VS.zipWith (++) newCascades stgs.cascades
                  direct = stgs.direct
                  overlay = stgs.overlay
                  decals = stgs.decals
              in Stages {..}

        -- For any batches with the same DrawConfig and mesh member and ordering, we can combine the instances
        batchInstanceSignature (DrawConfig {..}, meshMemberName, ordering, _) =
          ( material.uuid, (.uuid) <$> perDrawDescriptorSet, meshCriteria, ordering, meshMemberName)
          where
          meshCriteria = case mesh of
            Buffered m -> (m.uuid, Equal)
            Dynamic _ -> (UUID.nil, NonEqual)
        compactBatches :: [Stages (DrawConfig, Text, Int, [Word32])]
        compactBatches = batches <&> mapStagesLists \items -> flip mapMaybe (bucketOn batchInstanceSignature items) \group ->
          headMay group <&> \(drawConfig, meshMemberName, ordering, _) -> (drawConfig, meshMemberName, ordering, concat (toListOf (each . _4) group))
        totalBatchIds :: [(DrawConfig, [Word32])]
        totalBatchIds = flip mapMaybe compactBatches \stgs ->
          let flat = concatStages stgs
              drawConfig = view _1 <$> headMay flat
          in (,concatMap (view _4) flat) <$> drawConfig
        encodedBatches :: [Stages (DrawConfig, DrawBatch)]
        encodedBatches = compactBatches <&> mapStagesAccumOffset \offset (drawConfig, meshMemberName, ordering, batch) ->
          (fromIntegral (length batch), (drawConfig, DrawBatch { firstInstanceIndex = offset
                                                               , numInstances = fromIntegral (length batch)
                                                               , meshSelector = meshMemberName
                                                               , pushConst = 0
                                                               , ordering = ordering
                                                               }))
        stages :: Stages [(DrawConfig, DrawBatch)]
        stages = transposeStages encodedBatches
               & over #cascades (VS.imap \(fromIntegral . getFinite -> i) as -> set (each . each . _2 . #pushConst) i as)

    -- Off y' go to the GPU
    for_ totalBatchIds \(DrawConfig {..}, ids) -> do
      let MaterialDescriptorSet { instancesBuffer } = resourceForFrame swapchainImageIndex materialDescriptor
          DataBuffer {..} = instancesBuffer
      liftIO $ withMappedMemory allocator allocation bracket \bufptr -> do
        pokeArray (castPtr bufptr) ids

    let sortOpaque = concat . concatMap (bucketOn descId) . bucketOn meshId
        sortBlended = sortOn ((.ordering) . snd) . reverse -- The above pipeline tends to return in reverse order, so we reverse first
        meshId (DrawConfig {mesh}, _) = case mesh of
          Buffered bm -> bm.uuid
          _ -> UUID.nil
        descId (DrawConfig {perDrawDescriptorSet}, _) = maybe UUID.nil (.uuid) perDrawDescriptorSet

    logRef <- liftIO $ newIORef []
    let logger msg = liftIO $ modifyIORef' logRef (msg:)

    -- Stage 1 Shadows
    flip VS.imapM_ stages.cascades \(fromIntegral . getFinite -> i) bs ->
      useRenderConfig shadowRenderConfig commandBuffer [ DepthStencil (ClearDepthStencilValue 1 0) ] swapchainImageIndex (fst . (`VS.unsafeIndex` fromIntegral i) . fst <$> cascadedShadowMap) do
        logger $ printf "\n\nProcessing shadow cascade %d" (i :: Word32)
        processDrawCommands frameContext logger (concatMap sortOpaque bs)

    -- Stage 2 Current Selection
    useRenderConfig currentSelectionRenderConfig commandBuffer [ DepthStencil (ClearDepthStencilValue 1 0), Color (Uint32 0 0 0 0) ] swapchainImageIndex (fst <$> currentSelectionRenderFrame) do
      logger "\n\nProcessing current selection"
      processDrawCommands frameContext logger (concatMap sortOpaque stages.showSel)

    -- Stage 3 GBuffer
    let V4 r g b a = clearColor
    useRenderConfig gbufferRenderConfig commandBuffer
      [ Color (Float32 r g b a)
      , Color (Float32 1 1 1 1)
      , Color (Float32 0 0 0 0)
      , Color (Uint32 0 0 0 0)
      , DepthStencil (ClearDepthStencilValue 1 0)
      ] swapchainImageIndex gbufferRenderFrame do
      logger "\n\nProcessing gbuffer"
      processDrawCommands frameContext logger (concatMap sortOpaque stages.gbuf)

    -- Stage 4 Decals
    useRenderConfig decalRenderConfig commandBuffer [] swapchainImageIndex decalRenderFrame do
      logger "\n\nProcessing decals"
      processDrawCommands frameContext logger (concatMap sortBlended stages.decals)

    -- Stage 5 SSAO
    useRenderConfig ssaoRenderConfig commandBuffer [Color (Float32 0 0 0 0)] swapchainImageIndex (fst <$> ssaoRenderFrame) do
      cmdBindMaterial frameContext ssaoMaterial
      liftIO do
        cmdPushMaterialConstants commandBuffer ssaoMaterial ssaoSettings
        cmdDraw commandBuffer 3 1 0 0

    -- Stage 6 Lighting
    useRenderConfig lightingRenderConfig commandBuffer [Color (Float32 0 0 0 1)] swapchainImageIndex (fst <$> lightingRenderFrame) do
      -- Sun is a full screen light
      cmdBindMaterial frameContext sunMaterial
      liftIO do
        cmdBindDrawDescriptorSet commandBuffer sunMaterial (fromMaybe defaultEnvMapDescriptorSet envMap) -- per draw set so that it can be changed at runtime
        cmdPushMaterialConstants commandBuffer sunMaterial 0
        cmdDraw commandBuffer 3 1 0 0

    imageBarrier commandBuffer IMAGE_LAYOUT_UNDEFINED PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT ACCESS_COLOR_ATTACHMENT_WRITE_BIT
                               IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT ACCESS_COLOR_ATTACHMENT_WRITE_BIT
                               IMAGE_ASPECT_COLOR_BIT (resourceForFrame swapchainImageIndex postImage).image
    -- Stage 7 Post
    cmdUseRendering commandBuffer (zero
      { layerCount = 1
      , renderArea = Rect2D zero extent
      , colorAttachments =
        [ zero
          { imageView = (resourceForFrame swapchainImageIndex postImage).imageView
          , imageLayout = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
          , loadOp = ATTACHMENT_LOAD_OP_DONT_CARE
          , storeOp = ATTACHMENT_STORE_OP_STORE
          , clearValue = Color (Float32 0 0 0 1)
          }
        ]
      }) do
      cmdBindMaterial frameContext postProcessMaterial
      cmdBindDrawDescriptorSet commandBuffer postProcessMaterial (fromMaybe defaultLutDescriptorSet lut) -- per draw set so that it can be changed at runtime
      liftIO $ cmdPushMaterialConstants commandBuffer postProcessMaterial postSettings
      liftIO $ cmdDraw commandBuffer 3 1 0 0

    let currentDepthViewableImage = resourceForFrame swapchainImageIndex depthViewableImage
    -- We use depth as a texture for lighting, but need it for z-testing in forward rendering
    imageBarrier commandBuffer IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL (PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT .|. PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT) ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
                               IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL (PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT .|. PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT)
                                 (ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT .|. ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT)
                               IMAGE_ASPECT_DEPTH_BIT currentDepthViewableImage.image
    let imageReuseBarrier image =
          imageBarrier commandBuffer IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT ACCESS_COLOR_ATTACHMENT_WRITE_BIT
                                     IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                                       (ACCESS_COLOR_ATTACHMENT_READ_BIT .|. ACCESS_COLOR_ATTACHMENT_WRITE_BIT)
                                     IMAGE_ASPECT_COLOR_BIT image
    imageReuseBarrier (resourceForFrame swapchainImageIndex postImage).image

    -- Stage 8 Forward (aka 'Direct')
    cmdUseRendering commandBuffer (zero
      { layerCount = 1
      , renderArea = Rect2D zero extent
      , colorAttachments =
        [ zero
          { imageView = (resourceForFrame swapchainImageIndex postImage).imageView
          , imageLayout = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
          , loadOp = ATTACHMENT_LOAD_OP_LOAD
          , storeOp = ATTACHMENT_STORE_OP_STORE
          , clearValue = Color (Float32 0 0 0 1)
          }
        ]
      , depthAttachment = Just
        zero
          { imageView = currentDepthViewableImage.imageView
          , imageLayout = IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL
          , loadOp = ATTACHMENT_LOAD_OP_LOAD
          , storeOp = ATTACHMENT_STORE_OP_STORE
          , clearValue = DepthStencil (ClearDepthStencilValue 1 0)
          }
      }) do
      -- Layer in extra direct color commands
      logger "\n\nProcessing direct world"
      processDrawCommands frameContext logger (sortBlended $ concat stages.direct)

    imageBarrier commandBuffer IMAGE_LAYOUT_UNDEFINED PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT ACCESS_COLOR_ATTACHMENT_WRITE_BIT
                               IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT ACCESS_COLOR_ATTACHMENT_WRITE_BIT
                               IMAGE_ASPECT_COLOR_BIT (resourceForFrame swapchainImageIndex blurImage).image

    imageBarrier commandBuffer IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT ACCESS_COLOR_ATTACHMENT_WRITE_BIT
                               IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL PIPELINE_STAGE_FRAGMENT_SHADER_BIT ACCESS_SHADER_READ_BIT
                               IMAGE_ASPECT_COLOR_BIT (resourceForFrame swapchainImageIndex postImage).image

    -- Blur (for depth of field)
    for_ camera.depthOfField \_ -> cmdUseRendering commandBuffer (zero
      { layerCount = 1
      , renderArea = Rect2D zero blurExtent
      , colorAttachments =
        [ zero
          { imageView = (resourceForFrame swapchainImageIndex blurImage).imageView
          , imageLayout = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
          , loadOp = ATTACHMENT_LOAD_OP_DONT_CARE
          , storeOp = ATTACHMENT_STORE_OP_STORE
          , clearValue = Color (Float32 0 0 0 1)
          }
        ]
      }) do
        cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_GRAPHICS blurMaterial.pipeline
        cmdBindDescriptorSets commandBuffer PIPELINE_BIND_POINT_GRAPHICS blurMaterial.pipelineLayout 0
          [(resourceForFrame swapchainImageIndex blurDescriptorSet).descriptorSet] []
        cmdPushMaterialConstants commandBuffer blurMaterial (BlurConstants 1 2)
        cmdDraw commandBuffer 3 1 0 0

    imageBarrier commandBuffer IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT ACCESS_COLOR_ATTACHMENT_WRITE_BIT
                               IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL PIPELINE_STAGE_FRAGMENT_SHADER_BIT ACCESS_SHADER_READ_BIT
                               IMAGE_ASPECT_COLOR_BIT (resourceForFrame swapchainImageIndex blurImage).image

    imageBarrier commandBuffer IMAGE_LAYOUT_UNDEFINED PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT ACCESS_COLOR_ATTACHMENT_WRITE_BIT
                               IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT ACCESS_COLOR_ATTACHMENT_WRITE_BIT
                               IMAGE_ASPECT_COLOR_BIT colorImage.image

    imageBarrier commandBuffer IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL (PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT .|. PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT) ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
                               IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL PIPELINE_STAGE_FRAGMENT_SHADER_BIT ACCESS_SHADER_READ_BIT
                               IMAGE_ASPECT_DEPTH_BIT currentDepthViewableImage.image

    cmdUseRendering commandBuffer (zero
      { layerCount = 1
      , renderArea = Rect2D zero extent
      , colorAttachments =
        [ zero
          { imageView = colorImage.imageView
          , imageLayout = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
          , loadOp = ATTACHMENT_LOAD_OP_DONT_CARE
          , storeOp = ATTACHMENT_STORE_OP_STORE
          , clearValue = Color (Float32 0 0 0 1)
          }
        ]
      }) do
        cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_GRAPHICS depthOfFieldMaterial.pipeline
        cmdBindDescriptorSets commandBuffer PIPELINE_BIND_POINT_GRAPHICS depthOfFieldMaterial.pipelineLayout 0
          [ (resourceForFrame swapchainImageIndex globalDescriptorSet).descriptorSet
          , (resourceForFrame swapchainImageIndex depthOfFieldDescriptorSet).descriptorSet
          ] []
        cmdPushMaterialConstants commandBuffer depthOfFieldMaterial (fromMaybe (DepthOfFieldConstants 0 0 0) camera.depthOfField)
        cmdDraw commandBuffer 3 1 0 0

    imageReuseBarrier colorImage.image

    -- UI (No depth)
    cmdUseRendering commandBuffer (zero
      { layerCount = 1
      , renderArea = Rect2D zero extent
      , colorAttachments =
        [ zero
          { imageView = colorImage.imageView
          , imageLayout = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
          , loadOp = ATTACHMENT_LOAD_OP_LOAD
          , storeOp = ATTACHMENT_STORE_OP_STORE
          , clearValue = Color (Float32 0 0 0 1)
          }
        ]
      }) do

      case highlightObjs of
        (_:_) -> do
          cmdBindMaterial frameContext objHighlightMaterial
          liftIO $ cmdDraw commandBuffer 3 1 0 0
        _ -> pure ()

      logger "\n\nProcessing overlay"
      processDrawCommands frameContext logger (sortBlended $ concat stages.overlay)

    -- Barrier before ImGUI gets a hold of it
    imageReuseBarrier colorImage.image

    -- Make object picking texture available for reading
    liftIO $ copyDescriptorImageToBuffer commandBuffer (resourceForFrame swapchainImageIndex objectPickingImageBuffer)

    msgs <- liftIO $ unlines . reverse <$> readIORef logRef
    pure Stats
      { numLitDraws     = length worldDrawCommands
      , numOverlayDraws = length overlayDrawCommands
      , numGBuffer = 0
      , numGBufferInstances = 0
      , numGBufferPostCullInstances = 0 -- sum $ sum . fmap (snd . fst) <$> gbufStageDCs
      , numCastingShadows = 0 -- TODO sum $ length <$> dcsCastingShadows
      , numInstancesPerCascade = [0] -- (\m -> sum $ sum . fmap (snd . fst) <$> m) <$> cascadeDCs
      , numDirect = 0
      , logMessages = msgs
      }

meshBoundingSphere :: M44 Scalar -> Text -> MeshType -> (V3 Float, Float)
meshBoundingSphere modelMat meshMemberName mesh = (center, norm (max' - center))
  where
  (min', max') = case mesh of
    Dynamic  m -> (transformV3 modelMat m.minPosition, transformV3 modelMat m.maxPosition)
    Buffered m -> let meshMember = HashMap.lookupDefault (head $ HashMap.elems m.members) meshMemberName m.members
                  in (transformV3 modelMat meshMember.minPosition, transformV3 modelMat meshMember.maxPosition)
  center = glerp 0.5 min' max'

bindMaterialIfNeeded :: (MonadState UUID m, MonadIO m) => FrameContext -> Material Word32 -> m ()
bindMaterialIfNeeded fc material = do
  curUUID <- get
  when (curUUID /= material.uuid) do
    cmdBindMaterial fc material
    put curUUID

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
    { instances       = [("", [(0, mat)])]
    , mesh            = Dynamic (textMesh font tc)
    , materialConfig  = materialConfig
    , doCastShadow    = False
    , doBlend         = True
    , descriptorSet   = Just fontTex
    , cull = False
    , pokeData = \_ -> flip poke $ MSDFMatConstants
        { modelMat      = mat
        , color         = color
        , outlineColor  = outlineColor
        , outlineSize   = outlineSize
        , sdfPixelRange = sdfPixelRange
        , tiling        = V2 1 1
        }
    }

pickObjectID :: FrameContext -> Renderer -> (Scalar,Scalar) -> IO Word32
pickObjectID FrameContext {..} Renderer{..} = fmap fromIntegral . readPixel (resourceForFrame (swapchainImageIndex - 1) objectPickingImageBuffer)

processDrawCommands
  :: (MonadIO m, DynamicMeshMonad m)
  => FrameContext
  -> (String -> IO ())
  -> [(DrawConfig, DrawBatch)]
  -> m ()
processDrawCommands fc@FrameContext {..} logger batches = do
  curMatRef <- liftIO $ newIORef UUID.nil
  curMeshRef <- liftIO $ newIORef UUID.nil
  curPerDrawDescriptorRef <- liftIO $ newIORef UUID.nil

  for_ batches \(DrawConfig {..}, DrawBatch {..}) -> do
    curMat <- liftIO $ readIORef curMatRef
    curMesh <- liftIO $ readIORef curMeshRef

    when (curMat /= material.uuid) do
      liftIO do
        logger $ "Binding material: " ++ show material.uuid
        writeIORef curMatRef material.uuid
        writeIORef curPerDrawDescriptorRef UUID.nil
        cmdBindMaterial fc material

    curPerDrawDescriptor <- liftIO $ readIORef curPerDrawDescriptorRef

    case perDrawDescriptorSet of
      Just pds | hasPerDrawDescriptorSet material && pds.uuid /= curPerDrawDescriptor -> do
        liftIO do
          logger $ "Binding per-draw descriptor set: " ++ show pds.uuid
          writeIORef curPerDrawDescriptorRef pds.uuid
        cmdBindDrawDescriptorSet commandBuffer material pds
      _ -> pure ()

    liftIO $ with pushConst $ cmdPushConstants commandBuffer material.pipelineLayout (SHADER_STAGE_VERTEX_BIT .|. SHADER_STAGE_FRAGMENT_BIT) 0 (fromIntegral $ sizeOf (undefined :: Word32)) . castPtr

    case mesh of
      Buffered BufferedMesh {..} -> do
        when (uuid /= curMesh) do
          liftIO do
            logger $ "Binding mesh: " ++ show uuid
            writeIORef curMeshRef uuid
          let bindOffsets = V.fromList $ material.attributes <&> \a ->
                fromIntegral . fromMaybe (error $ "Can't find attribute '" ++ show a ++ "' in mesh " ++ fromMaybe "" name)
                $ Prelude.lookup a meshOffsets
              vertexBuffers = V.fromList $ vertexBuffer <$ material.attributes
          cmdBindVertexBuffers commandBuffer 0 vertexBuffers bindOffsets
          for_ indexBuffer \ibuf ->
            cmdBindIndexBuffer commandBuffer ibuf 0 INDEX_TYPE_UINT32

        let BufferedMeshMember {..} = HashMap.lookupDefault (head $ HashMap.elems members)  meshSelector members
        case indexCount of
          Just n -> do
            liftIO . logger $ printf "Drawing indexed %d instances" numInstances
            cmdDrawIndexed commandBuffer n numInstances (fromMaybe 0 firstIndex) (fromIntegral vertexOffset) firstInstanceIndex
          Nothing -> do
            liftIO . logger $ printf "Drawing %d instances" numInstances
            cmdDraw commandBuffer vertexCount numInstances vertexOffset firstInstanceIndex
      Dynamic (Mesh { vertices = [] }) -> pure ()
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
        liftIO . logger $ "Drawing dynamic"

        let
            bindOffsets = V.fromList $ material.attributes <&> \a -> fromIntegral . (+vertexSizeThusFar) . fromIntegral . fromMaybe (error $ "Can't find attribute '" ++ show a ++ "' in dynamic mesh")
                                                          $ Prelude.lookup a meshOffsets
            vertexBuffers = V.fromList $ vertexBuffer <$ material.attributes

        cmdBindVertexBuffers commandBuffer 0 vertexBuffers bindOffsets

        case numIndices of
          Just n -> do
            cmdBindIndexBuffer commandBuffer indexBuffer (fromIntegral indexSizeThusFar) INDEX_TYPE_UINT32
            cmdDrawIndexed commandBuffer n numInstances 0 0 firstInstanceIndex
          Nothing -> do
            cmdDraw commandBuffer numVertices numInstances 0 firstInstanceIndex

        liftIO $ writeIORef curMeshRef UUID.nil


debugBarrier :: MonadIO io => CommandBuffer -> io ()
debugBarrier commandBuffer =
  cmdPipelineBarrier commandBuffer PIPELINE_STAGE_ALL_COMMANDS_BIT PIPELINE_STAGE_ALL_COMMANDS_BIT zero
    [zero
      { srcAccessMask = ACCESS_MEMORY_READ_BIT .|. ACCESS_MEMORY_WRITE_BIT
      , dstAccessMask = ACCESS_MEMORY_READ_BIT .|. ACCESS_MEMORY_WRITE_BIT
      }] [] []
