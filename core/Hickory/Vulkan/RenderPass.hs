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
  , cmdNextSubpass, RenderPass, Filter (..), SamplerAddressMode (..)
  )
import Vulkan.Zero
import Acquire.Acquire (Acquire)
import qualified Data.Vector as V
import Data.Generics.Labels ()
import Data.Traversable (for)
import Hickory.Vulkan.Textures (withIntermediateImage, withImageSampler)
import Data.Bits ((.|.), zeroBits)
import Hickory.Vulkan.Monad (FrameMonad (askFrameContext), CommandT, recordCommandBuffer)
import Hickory.Vulkan.Material (Material, withMaterial, cmdBindMaterial, cmdPushMaterialConstants)
import Hickory.Vulkan.Framing (FramedResource(..), doubleResource)
import Hickory.Vulkan.DescriptorSet (PointedDescriptorSet(..), withTexturesDescriptorSet)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag, vert)
import Linear (V4 (..), V3)
import Control.Monad.IO.Class (MonadIO)
import Hickory.Math (Scalar)
import Hickory.Vulkan.Frame (FrameContext(..))
import GHC.Generics (Generic)
import Foreign.Storable.Generic (GStorable)
import Data.Proxy (Proxy)

data RenderTarget = RenderTarget
  { renderPass          :: !RenderPass
  , frameBuffers        :: !(V.Vector Framebuffer)
  , postProcessMaterial :: !(Material PostConstants)
  }

withStandardRenderTarget :: VulkanResources -> Swapchain -> Acquire RenderTarget
withStandardRenderTarget vulkanResources@VulkanResources {deviceContext = deviceContext@DeviceContext{..}} swapchain@Swapchain {..} = do
  renderPass <- withRenderPass device zero
    { attachments  = [shadowmapAttachmentDescription, hdrColorAttachmentDescription, depthAttachmentDescription, resolveAttachmentDescription, outColorAttachmentDescription]
    , subpasses    = [shadowSubpass, litSubpass, postOverlaySubpass]
    , dependencies = [shadowDependency, litDependency, postOverlayDependency]
    } Nothing mkAcquire

  -- Shadowmap depth texture
  shadowmapImageRaw  <- withDepthImage vulkanResources extent depthFormat SAMPLE_COUNT_1_BIT IMAGE_USAGE_SAMPLED_BIT
  shadowmapImageView <- with2DImageView deviceContext depthFormat IMAGE_ASPECT_DEPTH_BIT shadowmapImageRaw
  let _showmapImage = ViewableImage shadowmapImageRaw shadowmapImageView depthFormat

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

  frameBuffers <- for images \(ViewableImage _img imgView _format) ->
    createFramebuffer device renderPass extent [shadowmapImageView, hdrImageView, depthImageView, resolveImageView, imgView]

  sampler <- withImageSampler vulkanResources FILTER_LINEAR SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
  descriptorSet <- withTexturesDescriptorSet vulkanResources
    [ (resolveImage, sampler)
    ]

  postProcessMaterial <- withPostProcessMaterial vulkanResources swapchain renderPass (doubleResource descriptorSet)

  pure RenderTarget {..}

  where
  resolveFormat = hdrFormat
  hdrFormat     = FORMAT_R16G16B16A16_SFLOAT
  depthFormat   = FORMAT_D32_SFLOAT
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
    , finalLayout    = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
    }
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
  shadowSubpass :: SubpassDescription
  shadowSubpass = zero
    { pipelineBindPoint = PIPELINE_BIND_POINT_GRAPHICS
    , depthStencilAttachment = Just $ zero
      { attachment = 0
      , layout     = IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
      }
    }
  litSubpass :: SubpassDescription
  litSubpass = zero
    { pipelineBindPoint = PIPELINE_BIND_POINT_GRAPHICS
    , colorAttachments =
      [ zero
        { attachment = 1
        , layout     = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        }
      ]
    , depthStencilAttachment = Just $ zero
      { attachment = 2
      , layout     = IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
      }
    , resolveAttachments =
      [ zero
        { attachment = 3
        , layout     = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        }
      ]
    }
  postOverlaySubpass :: SubpassDescription
  postOverlaySubpass = zero
    { pipelineBindPoint = PIPELINE_BIND_POINT_GRAPHICS
    , colorAttachments =
      [ zero
        { attachment = 4
        , layout     = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        }
      ]
    , depthStencilAttachment = Nothing
    , inputAttachments =
      [ zero
        { attachment = 3
        , layout     = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
        }
      ]
    }
  shadowDependency :: SubpassDependency
  shadowDependency = zero
    { srcSubpass    = SUBPASS_EXTERNAL
    , dstSubpass    = 0
    , srcStageMask  = PIPELINE_STAGE_FRAGMENT_SHADER_BIT
    , srcAccessMask = ACCESS_SHADER_READ_BIT
    , dstStageMask  = PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
    , dstAccessMask = ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
    }
  litDependency :: SubpassDependency
  litDependency = zero
    { srcSubpass    = 0
    , dstSubpass    = 1
    , srcStageMask  = PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
    , srcAccessMask = zero
    , dstStageMask  = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    , dstAccessMask = ACCESS_COLOR_ATTACHMENT_READ_BIT .|. ACCESS_COLOR_ATTACHMENT_WRITE_BIT
    }
  postOverlayDependency :: SubpassDependency
  postOverlayDependency = zero
    { srcSubpass    = 1
    , dstSubpass    = 2
    , srcStageMask  = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    , srcAccessMask = ACCESS_COLOR_ATTACHMENT_WRITE_BIT
    , dstStageMask  = PIPELINE_STAGE_FRAGMENT_SHADER_BIT
    , dstAccessMask = ACCESS_SHADER_READ_BIT
    }

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

data PostConstants = PostConstants
  { exposure    :: Float
  , colorShift  :: V3 Float
  , saturation  :: Float
  , filmGrain   :: Float
  , frameNumber :: Int
  } deriving Generic
    deriving anyclass GStorable

withPostProcessMaterial :: VulkanResources -> Swapchain -> RenderPass -> FramedResource PointedDescriptorSet -> Acquire (Material PostConstants)
withPostProcessMaterial vulkanResources swapchain renderPass materialDescriptorSet =
  withMaterial vulkanResources swapchain renderPass (undefined :: Proxy PostConstants)
    [] PRIMITIVE_TOPOLOGY_TRIANGLE_LIST vertShader fragShader [materialDescriptorSet] Nothing
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

layout (set = 0, binding = 0) uniform sampler2D textureSampler;

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

renderToTarget :: (FrameMonad m, MonadIO m) => RenderTarget -> V4 Scalar -> PostConstants -> CommandT m () -> CommandT m () -> m ()
renderToTarget RenderTarget {..} (V4 r g b a) postConstants litF overlayF = do
  FrameContext {..} <- askFrameContext

  let framebuffer = frameBuffers V.! fromIntegral swapchainImageIndex
      renderPassBeginInfo = zero
        { renderPass  = renderPass
        , framebuffer = framebuffer
        , renderArea  = Rect2D { offset = zero , extent = extent }
        , clearValues = [ DepthStencil (ClearDepthStencilValue 1 0)
                        , Color (Float32 r g b a)
                        , DepthStencil (ClearDepthStencilValue 1 0)
                        , Color (Float32 1 1 1 1)
                        ]
        }
  cmdUseRenderPass commandBuffer renderPassBeginInfo SUBPASS_CONTENTS_INLINE do
    cmdNextSubpass commandBuffer SUBPASS_CONTENTS_INLINE
    recordCommandBuffer 1 do
      litF

    cmdNextSubpass commandBuffer SUBPASS_CONTENTS_INLINE

    cmdBindMaterial frameNumber 2 commandBuffer postProcessMaterial
    cmdPushMaterialConstants commandBuffer postProcessMaterial postConstants
    cmdDraw commandBuffer 3 1 0 0

    recordCommandBuffer 2 do
      overlayF
