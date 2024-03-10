{-# LANGUAGE PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies, TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE DataKinds, OverloadedLists #-}

module Hickory.Vulkan.Renderer.Direct where

import Hickory.Vulkan.Vulkan (mkAcquire)
import Vulkan
  ( Format (..)
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


  , Filter (..), SamplerAddressMode (..), Framebuffer, SamplerMipmapMode (..)
  )
import Vulkan.Zero
import Acquire.Acquire (Acquire)
import Data.Generics.Labels ()
import Hickory.Vulkan.Textures (withImageSampler)
import Data.Bits ((.|.))
import Hickory.Vulkan.Types
import Hickory.Vulkan.RenderPass (createFramebuffer)
import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (compileShaderQ)
import Data.String.QM (qm)
import Hickory.Vulkan.Renderer.ShaderDefinitions
import Hickory.Vulkan.Renderer.GBuffer (depthFormat)

hdrFormat :: Format
hdrFormat = FORMAT_R16G16B16A16_SFLOAT

withDirectFrameBuffer :: VulkanResources -> RenderConfig -> ViewableImage -> ViewableImage -> Acquire (Framebuffer, [DescriptorSpec])
withDirectFrameBuffer vulkanResources@VulkanResources { deviceContext = DeviceContext{..} } RenderConfig {..} colorImage depthImage = do
  sampler <- withImageSampler vulkanResources FILTER_LINEAR SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE SAMPLER_MIPMAP_MODE_LINEAR

  let ViewableImage _ colorImageView _ = colorImage
      ViewableImage _ depthImageView _ = depthImage
  let descriptorSpecs = [ ImageDescriptor [(colorImage,sampler)]
                        , ImageDescriptor [(depthImage,sampler)]
                        ]
  (,descriptorSpecs) <$> createFramebuffer device renderPass extent [colorImageView, depthImageView]

withDirectRenderConfig :: VulkanResources -> Swapchain -> Acquire RenderConfig
withDirectRenderConfig VulkanResources { deviceContext = DeviceContext{..} } Swapchain {..} = do
  renderPass <- withRenderPass device zero
    { attachments  = [hdrAttachmentDescription, depthAttachmentDescription ]
    , subpasses    = [subpass]
    , dependencies = [dependency]
    } Nothing mkAcquire

  let samples = SAMPLE_COUNT_1_BIT
  pure RenderConfig {..}
  where
  hdrAttachmentDescription :: AttachmentDescription
  hdrAttachmentDescription = zero
    { format         = hdrFormat
    , samples        = SAMPLE_COUNT_1_BIT
    , loadOp         = ATTACHMENT_LOAD_OP_LOAD
    , storeOp        = ATTACHMENT_STORE_OP_STORE
    , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
    , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
    , initialLayout  = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
    , finalLayout    = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
    }
  depthAttachmentDescription :: AttachmentDescription
  depthAttachmentDescription = zero
    { format         = depthFormat
    , samples        = SAMPLE_COUNT_1_BIT
    , loadOp         = ATTACHMENT_LOAD_OP_LOAD
    , storeOp        = ATTACHMENT_STORE_OP_STORE
    , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
    , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
    , initialLayout  = IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
    , finalLayout    = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
    }
  subpass :: SubpassDescription
  subpass = zero
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
    }
  dependency :: SubpassDependency
  dependency = zero
    { srcSubpass    = SUBPASS_EXTERNAL
    , dstSubpass    = 0
    , srcStageMask  = PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT .|. PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
    , srcAccessMask = ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
    , dstStageMask  = PIPELINE_STAGE_FRAGMENT_SHADER_BIT
    , dstAccessMask = ACCESS_SHADER_READ_BIT
    }

staticDirectVertShader :: String
staticDirectVertShader = [qm|
$pushConstantsDef
$staticUniformsDef

layout(location = 0) in vec3 inPosition;
layout(location = 3) in vec2 inTexCoord;

layout(location = 0) out vec2 texCoord;
layout(location = 2) out vec4 color;

void main() {
  vec4 worldPosition = uniforms.modelMat * vec4(inPosition, 1.0);

  gl_Position = globals.viewProjMat
              * worldPosition;

  texCoord = inTexCoord;
  color = uniforms.color;
}

|]

staticDirectFragShader :: ByteString
staticDirectFragShader = $(compileShaderQ Nothing "frag" Nothing [qm|
$header
$worldGlobalsDef
$pushConstantsDef

layout(location = 0) in vec2 inTexCoord;
layout(location = 2) in vec4 inColor;
layout (set = 2, binding = 0) uniform sampler2D texSampler;

layout(location = 0) out vec4 outColor;

void main() {
  vec4 texColor = texture(texSampler, inTexCoord);
  outColor   = vec4(texColor * inColor);
}
|])
