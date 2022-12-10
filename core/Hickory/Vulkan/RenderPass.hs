{-# LANGUAGE PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes, DerivingStrategies #-}
{-# LANGUAGE DataKinds, DeriveGeneric, DeriveAnyClass, OverloadedLists, OverloadedLabels #-}

module Hickory.Vulkan.RenderPass where

-- With VK_KHR_dynamic_rendering, we might not actually need to create
-- renderpasses and framebuffers
-- (Only needed really if we need to take advantage of subpasses for
-- performance benefits.)

import Hickory.Vulkan.Vulkan (mkAcquire, ViewableImage(..), Swapchain (..), VulkanResources (..), DeviceContext (..), withDepthImage, with2DImageView)
import qualified Vulkan
import Vulkan
  ( Device
  , Extent2D (..)
  , Format (..)
  , withRenderPass
  , SampleCountFlagBits (..)
  , AttachmentDescription(..)
  , SubpassDescription(..)
  , SubpassDependency(..)
  , RenderPassCreateInfo(..)
  , AttachmentReference(..)
  , AttachmentLoadOp (..)
  , AttachmentStoreOp (..)
  , ImageLayout (..)
  , PipelineBindPoint (..)
  , pattern SUBPASS_EXTERNAL
  , PipelineStageFlagBits (..)
  , AccessFlagBits (..)
  , FramebufferCreateInfo(..)
  , ImageView
  , Framebuffer
  , withFramebuffer
  , SurfaceFormatKHR, ImageAspectFlagBits (..)
  , ImageUsageFlagBits(..)
  , ClearValue(..)
  , ClearColorValue(..)
  , cmdDraw, PrimitiveTopology(..), Rect2D (..), RenderPassBeginInfo(..)
  , cmdUseRenderPass
  , ClearDepthStencilValue(..)
  , pattern SUBPASS_CONTENTS_INLINE
  , cmdNextSubpass, RenderPass, Filter (..), SamplerAddressMode (..), Sampler, cmdBindDescriptorSets, cmdBindPipeline, BufferUsageFlagBits (..), MemoryPropertyFlagBits (..), Buffer, CullModeFlagBits (..)
  )
import Vulkan.Zero
import Acquire.Acquire (Acquire)
import qualified Data.Vector as V
import Data.Generics.Labels ()
import Data.Traversable (for)
import Hickory.Vulkan.Textures (withIntermediateImage, withImageSampler, withShadowSampler)
import Data.Bits ((.|.), zeroBits)
import Hickory.Vulkan.Monad (FrameMonad (askFrameContext), CommandT, recordCommandBuffer, shadowMap, meshOptions, DrawCommand (..), blend)
import Hickory.Vulkan.Material (withMaterial, cmdBindMaterial, cmdPushMaterialConstants, shadowDim, pipelineDefaults)
import Hickory.Vulkan.Framing (FramedResource(..), doubleResource)
import Hickory.Vulkan.DescriptorSet (withDescriptorSet)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag, vert)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Hickory.Math (Scalar, Mat44)
import Hickory.Vulkan.Frame (FrameContext(..))
import Data.Proxy (Proxy)
import Hickory.Vulkan.Types
import Linear.V3 (V3)
import GHC.Generics (Generic)
import Foreign.Storable.Generic (GStorable)
import Control.Lens (view)
import Hickory.Vulkan.Mesh (withBuffer')
import Foreign (sizeOf, poke)
import VulkanMemoryAllocator (withMappedMemory, Allocator, Allocation)
import Control.Exception (bracket)
import Foreign.Ptr (castPtr)
import Data.List (partition, sortOn)
import Control.Monad (when)
import Data.Foldable (for_)
import Linear ( V4(..), V3(..), identity)
import qualified Data.ByteString as B
import Data.Maybe

depthFormat :: Format
depthFormat = FORMAT_D32_SFLOAT

withShadowRenderTarget :: VulkanResources -> Acquire RenderTarget
withShadowRenderTarget vulkanResources@VulkanResources { deviceContext = deviceContext@DeviceContext{..} } = do
  renderPass <- withRenderPass device zero
    { attachments  = [shadowmapAttachmentDescription]
    , subpasses    = [shadowSubpass]
    , dependencies = [shadowDependency]
    } Nothing mkAcquire

  -- Shadowmap depth texture
  shadowmapImageRaw  <- withDepthImage vulkanResources shadowDim depthFormat SAMPLE_COUNT_1_BIT (IMAGE_USAGE_SAMPLED_BIT .|. IMAGE_USAGE_INPUT_ATTACHMENT_BIT)
  shadowmapImageView <- with2DImageView deviceContext depthFormat IMAGE_ASPECT_DEPTH_BIT shadowmapImageRaw
  let image = ViewableImage shadowmapImageRaw shadowmapImageView depthFormat
  sampler <- withShadowSampler vulkanResources

  shadowFrameBuffer <- createFramebuffer device renderPass shadowDim [shadowmapImageView]
  let frameBuffers = V.replicate 3 shadowFrameBuffer
      descriptorSpec = DepthImageDescriptor image sampler
      extent = shadowDim
      cullMode = CULL_MODE_FRONT_BIT
      samples = SAMPLE_COUNT_1_BIT
      fragShaderOverride = Just shadowFragShader

  pure RenderTarget {..}
  where
  shadowSubpass :: SubpassDescription
  shadowSubpass = zero
    { pipelineBindPoint = PIPELINE_BIND_POINT_GRAPHICS
    , depthStencilAttachment = Just $ zero
      { attachment = 0
      , layout     = IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
      }
    }
  shadowDependency :: SubpassDependency
  shadowDependency = zero
    { srcSubpass    = SUBPASS_EXTERNAL
    , dstSubpass    = 0
    , srcStageMask  = PIPELINE_STAGE_FRAGMENT_SHADER_BIT
    , srcAccessMask = zero
    , dstStageMask  = PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
    , dstAccessMask = ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
    }
  shadowmapAttachmentDescription :: AttachmentDescription
  shadowmapAttachmentDescription = zero
    { format         = depthFormat
    , samples        = SAMPLE_COUNT_1_BIT
    , loadOp         = ATTACHMENT_LOAD_OP_CLEAR
    , storeOp        = ATTACHMENT_STORE_OP_STORE
    , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
    , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
    , initialLayout  = IMAGE_LAYOUT_UNDEFINED
    , finalLayout    = IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL
    }
  -- For the shadowmap, we don't care about pixel color
  shadowFragShader :: B.ByteString
  shadowFragShader = [frag|
  #version 450

  layout(location = 0) out vec4 outColor;

  void main() {
    outColor = vec4(1.0,1.0,1.0,1.0);
  }
  |]

withLitRenderTarget :: VulkanResources -> Swapchain -> Acquire RenderTarget
withLitRenderTarget vulkanResources@VulkanResources { deviceContext = deviceContext@DeviceContext{..} } Swapchain {..} = do
  renderPass <- withRenderPass device zero
    { attachments  = [hdrColorAttachmentDescription, depthAttachmentDescription, resolveAttachmentDescription]
    , subpasses    = [litSubpass]
    , dependencies = [litDependency]
    } Nothing mkAcquire

  -- Target textures for the lit pass
  hdrImageRaw  <- withIntermediateImage vulkanResources hdrFormat (IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. IMAGE_USAGE_INPUT_ATTACHMENT_BIT) extent maxSampleCount
  hdrImageView <- with2DImageView deviceContext hdrFormat IMAGE_ASPECT_COLOR_BIT hdrImageRaw
  let _hdrImage = ViewableImage hdrImageRaw hdrImageView hdrFormat

  depthImageRaw  <- withDepthImage vulkanResources extent depthFormat maxSampleCount zeroBits
  depthImageView <- with2DImageView deviceContext depthFormat IMAGE_ASPECT_DEPTH_BIT depthImageRaw
  let _depthImage = ViewableImage depthImageRaw depthImageView depthFormat

  -- Target tex for the multisample resolve pass
  resolveImageRaw  <- withIntermediateImage vulkanResources resolveFormat
    (IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. IMAGE_USAGE_INPUT_ATTACHMENT_BIT)
    extent SAMPLE_COUNT_1_BIT
  resolveImageView <- with2DImageView deviceContext resolveFormat IMAGE_ASPECT_COLOR_BIT resolveImageRaw
  let resolveImage = ViewableImage resolveImageRaw resolveImageView resolveFormat

  frameBuffer <-
    createFramebuffer device renderPass extent [hdrImageView, depthImageView, resolveImageView]

  sampler <- withImageSampler vulkanResources FILTER_LINEAR SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE

  let frameBuffers = V.replicate 3 frameBuffer
      -- targetImages = V.fromList [(image, sampler)]
      descriptorSpec = ImageDescriptor [(resolveImage,sampler)]
      cullMode = CULL_MODE_BACK_BIT
      samples = maxSampleCount
      fragShaderOverride = Nothing
  pure RenderTarget {..}
  where
  resolveFormat = hdrFormat
  hdrFormat     = FORMAT_R16G16B16A16_SFLOAT
  hdrColorAttachmentDescription :: AttachmentDescription
  hdrColorAttachmentDescription = zero
    { format         = hdrFormat
    , samples        = maxSampleCount
    , loadOp         = ATTACHMENT_LOAD_OP_CLEAR
    , storeOp        = ATTACHMENT_STORE_OP_STORE
    , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
    , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
    , initialLayout  = IMAGE_LAYOUT_UNDEFINED
    , finalLayout    = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
    }
  depthAttachmentDescription :: AttachmentDescription
  depthAttachmentDescription = zero
    { format         = depthFormat
    , samples        = maxSampleCount
    , loadOp         = ATTACHMENT_LOAD_OP_CLEAR
    , storeOp        = ATTACHMENT_STORE_OP_DONT_CARE
    , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
    , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
    , initialLayout  = IMAGE_LAYOUT_UNDEFINED
    , finalLayout    = IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
    }
  resolveAttachmentDescription :: AttachmentDescription
  resolveAttachmentDescription = zero
    { format         = resolveFormat
    , samples        = SAMPLE_COUNT_1_BIT -- Resolve multisampling
    , loadOp         = ATTACHMENT_LOAD_OP_DONT_CARE
    , storeOp        = ATTACHMENT_STORE_OP_STORE
    , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
    , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
    , initialLayout  = IMAGE_LAYOUT_UNDEFINED
    , finalLayout    = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
    }
  litSubpass :: SubpassDescription
  litSubpass = zero
    { pipelineBindPoint = PIPELINE_BIND_POINT_GRAPHICS
    , colorAttachments =
      [ zero
        { attachment = 0
        , layout     = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        }
      ]
    , depthStencilAttachment = Just $ zero
      { attachment = 1
      , layout     = IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
      }
    , resolveAttachments =
      [ zero
        { attachment = 2
        , layout     = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        }
      ]
    }
  litDependency :: SubpassDependency
  litDependency = zero
    { srcSubpass    = SUBPASS_EXTERNAL
    , dstSubpass    = 0
    , srcStageMask  = PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT .|. PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
    , srcAccessMask = ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
    , dstStageMask  = PIPELINE_STAGE_FRAGMENT_SHADER_BIT
    , dstAccessMask = ACCESS_SHADER_READ_BIT
    }

withSwapchainRenderTarget :: VulkanResources -> Swapchain -> Acquire RenderTarget
withSwapchainRenderTarget VulkanResources { deviceContext = DeviceContext{..} } Swapchain {..} = do
  renderPass <- withRenderPass device zero
    { attachments  = [outColorAttachmentDescription]
    , subpasses    = [postOverlaySubpass]
    , dependencies = [postOverlayDependency]
    } Nothing mkAcquire

  frameBuffers <- for images \(ViewableImage _img imgView _format) ->
    createFramebuffer device renderPass extent [imgView]

  let descriptorSpec = undefined -- TODO: Handle swapchain targets more gracefully
      cullMode = CULL_MODE_BACK_BIT
      samples = SAMPLE_COUNT_1_BIT
      fragShaderOverride = Nothing

  pure RenderTarget {..}
  where
  outColorAttachmentDescription :: AttachmentDescription
  outColorAttachmentDescription = zero
    { format         = Vulkan.format (imageFormat :: SurfaceFormatKHR)
    , samples        = SAMPLE_COUNT_1_BIT
    , loadOp         = ATTACHMENT_LOAD_OP_DONT_CARE
    , storeOp        = ATTACHMENT_STORE_OP_STORE
    , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
    , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
    , initialLayout  = IMAGE_LAYOUT_UNDEFINED
    , finalLayout    = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL -- Leave as attachment for DearImgui, which will present it
    }
  postOverlaySubpass :: SubpassDescription
  postOverlaySubpass = zero
    { pipelineBindPoint = PIPELINE_BIND_POINT_GRAPHICS
    , colorAttachments =
      [ zero
        { attachment = 0
        , layout     = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        }
      ]
    , depthStencilAttachment = Nothing
    }
  postOverlayDependency :: SubpassDependency
  postOverlayDependency = zero
    { srcSubpass    = SUBPASS_EXTERNAL
    , dstSubpass    = 0
    , srcStageMask  = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    , srcAccessMask = ACCESS_COLOR_ATTACHMENT_WRITE_BIT
    , dstStageMask  = PIPELINE_STAGE_FRAGMENT_SHADER_BIT
    , dstAccessMask = ACCESS_SHADER_READ_BIT
    }

withForwardRenderTarget :: VulkanResources -> Swapchain -> [DescriptorSpec] -> Acquire ForwardRenderTarget
withForwardRenderTarget vulkanResources@VulkanResources {allocator} swapchain extraGlobalDescriptors = do
  shadowRenderTarget    <- withShadowRenderTarget vulkanResources
  litRenderTarget       <- withLitRenderTarget vulkanResources swapchain
  swapchainRenderTarget <- withSwapchainRenderTarget vulkanResources swapchain

  (buffer, alloc, _) <- withBuffer' allocator
    BUFFER_USAGE_UNIFORM_BUFFER_BIT
    (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
    (fromIntegral $ sizeOf (undefined :: Globals)) -- There's got to be a better way than hardcoding # of uniforms allowed
  let globalsBuffer = (buffer, alloc, allocator)

  globalDescriptorSet <- withDescriptorSet vulkanResources
    $ descriptorSpec shadowRenderTarget
    : BufferDescriptor buffer
    : extraGlobalDescriptors

  materialDescriptorSet <- withDescriptorSet vulkanResources
    [ view #descriptorSpec litRenderTarget
    ]

  postProcessMaterial <- withPostProcessMaterial vulkanResources [shadowRenderTarget, litRenderTarget, swapchainRenderTarget] (doubleResource globalDescriptorSet) (doubleResource materialDescriptorSet)

  pure ForwardRenderTarget {..}

createFramebuffer :: Device -> RenderPass -> Extent2D -> V.Vector ImageView -> Acquire Framebuffer
createFramebuffer dev renderPass swapchainExtent imageViews =
  let framebufferCreateInfo :: FramebufferCreateInfo '[]
      framebufferCreateInfo = zero
        { renderPass  = renderPass
        , attachments = imageViews
        , width       = width (swapchainExtent :: Extent2D)
        , height      = height (swapchainExtent :: Extent2D)
        , layers      = 1
        }
  in withFramebuffer dev framebufferCreateInfo Nothing mkAcquire

withPostProcessMaterial :: VulkanResources -> [RenderTarget] -> FramedResource PointedDescriptorSet -> FramedResource PointedDescriptorSet -> Acquire (Material PostConstants)
withPostProcessMaterial vulkanResources renderTargets globalDescriptorSet materialDescriptorSet =
  withMaterial vulkanResources renderTargets (undefined :: Proxy PostConstants)
    [] pipelineDefaults vertShader fragShader globalDescriptorSet materialDescriptorSet Nothing
  where
  vertShader = [vert|
#version 450

layout (location = 0) out vec2 texCoordsVarying;

void main()
{
    texCoordsVarying = vec2(gl_VertexIndex & 2, (gl_VertexIndex << 1) & 2 );
    gl_Position = vec4(texCoordsVarying * 2.0f + -1.0f, 1.0f, 1.0f);
}

|]
  fragShader = [frag|
#version 450
#extension GL_EXT_scalar_block_layout : require

layout (location = 0) in vec2 texCoordsVarying;
layout (location = 0) out vec4 outColor;

layout( push_constant, scalar ) uniform constants
{
  float exposure;
  vec3 colorShift;
  float saturation;
  float filmGrain;
  int frameNumber;
} PushConstants;

layout (set = 1, binding = 0) uniform sampler2D textureSampler;

// Bring hdr color into ldr range with an artistic curve
// Narkowicz 2015, "ACES Filmic Tone Mapping Curve"

vec3 aces_tonemapping(vec3 x) {
  const float a = 2.51;
  const float b = 0.03;
  const float c = 2.43;
  const float d = 0.59;
  const float e = 0.14;
  return clamp((x * (a * x + b)) / (x * (c * x + d) + e), 0.0, 1.0);
}

void main()
{
  lowp vec4 origColor = texture(textureSampler, texCoordsVarying);

  // Exposure
  vec3 exposureFilter = exp2(PushConstants.exposure) * PushConstants.colorShift;
  vec3 color = origColor.rgb * exposureFilter;

  // Saturation
  vec3 lumaWeights = vec3(0.25,0.50,0.25);
  float luminance = dot(lumaWeights, color.rgb);
  vec3 grey = vec3(luminance, luminance, luminance);
  vec3 saturated = grey + PushConstants.saturation * (color.rgb - grey);

  // Tonemapping
  color = aces_tonemapping(saturated);

  // Film grain
  float grainIntensity =
    fract( 10000
         * sin( (3.14 / 180)
              * ( texCoordsVarying.x * 360
                + texCoordsVarying.y * 36
                * PushConstants.frameNumber
                )
              )
         );

  color += PushConstants.filmGrain * grainIntensity;

  outColor = vec4(color, 1.0);
}
|]

data Globals = Globals
  { lightTransform :: Mat44
  , lightDirection :: V3 Scalar
  , sunColor       :: V3 Scalar -- HDR
  , ambientColor   :: V3 Scalar -- HDR
  } deriving Generic
    deriving anyclass GStorable

globalDefaults :: Globals
globalDefaults = Globals {..}
  where
  lightTransform = identity
  lightDirection = V3 1 1 1
  sunColor = V3 1 1 1
  ambientColor = V3 1 1 1

useRenderTarget :: (MonadIO io, Integral a) => RenderTarget -> Vulkan.CommandBuffer -> V.Vector ClearValue -> a -> io r -> io r
useRenderTarget RenderTarget {..} commandBuffer clearValues swapchainImageIndex f = do
  let framebuffer = fromMaybe (error "Error accessing framebuffer") $ frameBuffers V.!? fromIntegral swapchainImageIndex
      renderPassBeginInfo = zero
        { renderPass  = renderPass
        , framebuffer = framebuffer
        , renderArea  = Rect2D { offset = zero , extent = extent }
        , clearValues = clearValues
        }

  cmdUseRenderPass commandBuffer renderPassBeginInfo SUBPASS_CONTENTS_INLINE f

renderToForwardTarget :: (FrameMonad m, MonadIO m) => ForwardRenderTarget -> V4 Scalar -> Globals -> PostConstants -> CommandT m () -> CommandT m () -> m ()
renderToForwardTarget ForwardRenderTarget {..} (V4 r g b a) globals postConstants litF overlayF = do
  FrameContext {..} <- askFrameContext

  let (_,alloc,allocator) = globalsBuffer
  liftIO do
    withMappedMemory allocator alloc bracket \bufptr ->
      poke (castPtr bufptr) globals

  drawCommands <- recordCommandBuffer litF
  let (shadowCommands, litCommands) = partition (shadowMap . meshOptions) drawCommands

  useRenderTarget shadowRenderTarget commandBuffer [ DepthStencil (ClearDepthStencilValue 1 0) ] swapchainImageIndex do
    liftIO $ renderCommands commandBuffer frameNumber (selectPipeline 0) shadowCommands

  useRenderTarget litRenderTarget commandBuffer
    [ Color (Float32 r g b a)
    , DepthStencil (ClearDepthStencilValue 1 0)
    , Color (Float32 1 1 1 1)
    ] swapchainImageIndex do
      liftIO $ sortBlendedAndRenderCommands commandBuffer frameNumber (selectPipeline 1) litCommands

  useRenderTarget swapchainRenderTarget commandBuffer [] swapchainImageIndex do
    cmdBindMaterial frameNumber (selectPipeline 2) commandBuffer postProcessMaterial
    cmdPushMaterialConstants commandBuffer postProcessMaterial postConstants
    cmdDraw commandBuffer 3 1 0 0

    recordCommandBuffer overlayF >>= liftIO . sortBlendedAndRenderCommands commandBuffer frameNumber (selectPipeline 2)

  where
  selectPipeline i = fromMaybe (error $ "Error accessing pipeline: " ++ show i) . (V.!? i) . pipelines
  renderCommands commandBuffer frameNumber selector commands =
    for_ commands \DrawCommand{..} -> do
      cmdBindMaterial frameNumber selector commandBuffer material
      io commandBuffer
  sortBlendedAndRenderCommands commandBuffer frameNumber selector commands = do
    forState_ opaque Nothing \curMatId DrawCommand{..} -> do
      let newMatId = view #uuid material
      when (Just newMatId /= curMatId) do
        cmdBindMaterial frameNumber selector commandBuffer material
      io commandBuffer
      pure (Just newMatId)
    renderCommands commandBuffer frameNumber selector (reverse blended)
    where
    (blended, sortOn (view #uuid . material) . reverse -> opaque) = partition (blend . meshOptions) commands

forState_ :: Monad m => [t] -> a -> (a -> t -> m a) -> m ()
forState_ (x:xs) initialVal f = f initialVal x >>= flip (forState_ xs) f
forState_ [] _ _ = pure ()
