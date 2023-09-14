{-# LANGUAGE DataKinds, PatternSynonyms, QuasiQuotes, TemplateHaskell  #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric, DerivingStrategies, DeriveAnyClass, OverloadedLists, OverloadedLabels #-}

module Hickory.Vulkan.Forward.ObjectPicking where

import Hickory.Vulkan.Vulkan (mkAcquire, withDepthImage, with2DImageView)
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
  , ImageAspectFlagBits (..)
  , Filter (..), SamplerAddressMode (..), CullModeFlagBits (..), ImageUsageFlagBits (..)
  )
import Vulkan.Zero
import Acquire.Acquire (Acquire)
import Data.Generics.Labels ()
import Hickory.Vulkan.Textures (withIntermediateImage, withImageSampler)
import Data.Bits (zeroBits, Bits ((.|.)))
import Hickory.Vulkan.Material (pipelineDefaults, PipelineOptions(..), withMaterial)
import Hickory.Vulkan.Types
import Hickory.Vulkan.RenderPass (createFramebuffer)
import Hickory.Vulkan.Monad (BufferedUniformMaterial, withBufferedUniformMaterial)
import GHC.Generics (Generic)
import Foreign.Storable.Generic (GStorable)
import Linear (M44)
import Data.ByteString (ByteString)
import Data.Word (Word32)
import Hickory.Vulkan.Framing (FramedResource, frameResource)
import Data.Proxy (Proxy)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (compileShaderQ)
import Data.String.QM (qm)
import Hickory.Vulkan.Forward.ShaderDefinitions

-- For e.g. mouse picking objects in scene
withObjectIDRenderTarget :: VulkanResources -> Swapchain -> Acquire RenderTarget
withObjectIDRenderTarget vulkanResources@VulkanResources { deviceContext = deviceContext@DeviceContext{..} } Swapchain {..} = do
  renderPass <- withRenderPass device zero
    { attachments  = [depthDescription, objIDDescription]
    , subpasses    = [subpass]
    , dependencies = [dependency]
    } Nothing mkAcquire

  sampler <- withImageSampler vulkanResources FILTER_NEAREST SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE

  frameBuffers <- frameResource do
    depthImageRaw  <- withDepthImage vulkanResources extent depthFormat samples zeroBits
    depthImageView <- with2DImageView deviceContext depthFormat IMAGE_ASPECT_DEPTH_BIT depthImageRaw

    objIDImageRaw  <- withIntermediateImage vulkanResources objIDFormat (IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. IMAGE_USAGE_TRANSFER_SRC_BIT) extent samples
    objIDImageView <- with2DImageView deviceContext objIDFormat IMAGE_ASPECT_COLOR_BIT objIDImageRaw
    let objIDImage = ViewableImage objIDImageRaw objIDImageView objIDFormat

    let descriptorSpecs = [ ImageDescriptor [(objIDImage,sampler)]
                          ]
    (,descriptorSpecs) <$> createFramebuffer device renderPass extent [depthImageView, objIDImageView]

  let cullMode = CULL_MODE_BACK_BIT

  pure RenderTarget {..}
  where
  samples = SAMPLE_COUNT_1_BIT
  objIDFormat     = FORMAT_R16_UINT
  depthFormat = FORMAT_D16_UNORM
  subpass :: SubpassDescription
  subpass = zero
    { pipelineBindPoint = PIPELINE_BIND_POINT_GRAPHICS
    , colorAttachments =
      [ zero
        { attachment = 1
        , layout     = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        }
      ]
    , depthStencilAttachment = Just $ zero
      { attachment = 0
      , layout     = IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
      }
    }
  dependency :: SubpassDependency
  dependency = zero
    { srcSubpass    = SUBPASS_EXTERNAL
    , dstSubpass    = 0
    , srcStageMask  = PIPELINE_STAGE_FRAGMENT_SHADER_BIT
    , srcAccessMask = zero
    , dstStageMask  = PIPELINE_STAGE_FRAGMENT_SHADER_BIT
    , dstAccessMask = ACCESS_SHADER_READ_BIT
    }
  depthDescription :: AttachmentDescription
  depthDescription = zero
    { format         = depthFormat
    , samples        = samples
    , loadOp         = ATTACHMENT_LOAD_OP_CLEAR
    , storeOp        = ATTACHMENT_STORE_OP_DONT_CARE
    , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
    , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
    , initialLayout  = IMAGE_LAYOUT_UNDEFINED
    , finalLayout    = IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL
    }
  objIDDescription :: AttachmentDescription
  objIDDescription = zero
    { format         = objIDFormat
    , samples        = samples
    , loadOp         = ATTACHMENT_LOAD_OP_CLEAR
    , storeOp        = ATTACHMENT_STORE_OP_STORE
    , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
    , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
    , initialLayout  = IMAGE_LAYOUT_UNDEFINED
    , finalLayout    = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
    -- , finalLayout    = IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
    }

data ObjectIDConstants = ObjectIDConstants
  { modelMat :: M44 Float
  , objectID :: Word32
  } deriving Generic
    deriving anyclass GStorable

withObjectIDMaterial :: VulkanResources -> RenderTarget -> FramedResource PointedDescriptorSet -> Acquire (BufferedUniformMaterial ObjectIDConstants)
withObjectIDMaterial vulkanResources renderTarget globalDS
  = withBufferedUniformMaterial vulkanResources renderTarget [Position] (pipelineDefaults { blendEnable = False }) vertShader fragShader globalDS Nothing
  where
  vertShader :: ByteString
  vertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
$header
$worldGlobalsDef

layout(location = 0) in vec3 inPosition;
layout(location = 0) out uint objectID;

struct Uniforms
{
  mat4 modelMat;
  uint objectID;
};

layout (push_constant) uniform constants { uint uniformIdx; } PushConstants;
layout (row_major, scalar, set = 1, binding = 0) uniform UniformBlock { Uniforms uniforms [128]; } uniformBlock;

void main() {
    Uniforms uniforms = uniformBlock.uniforms[PushConstants.uniformIdx];
    gl_Position = globals.projMat
                * globals.viewMat
                * uniforms.modelMat
                * vec4(inPosition, 1.0);
    objectID = uniforms.objectID;
}

|])

  fragShader :: ByteString
  fragShader = $(compileShaderQ Nothing "frag" Nothing [qm|
$header

layout(location = 0) flat in uint objectID;
layout(location = 0) out uint outColor;

void main() {
  outColor = objectID;
}
|])

withObjectHighlightMaterial :: VulkanResources -> RenderTarget -> FramedResource PointedDescriptorSet -> FramedResource PointedDescriptorSet -> Acquire (Material Word32)
withObjectHighlightMaterial vulkanResources renderTarget globalDescriptorSet materialDescriptorSet =
  withMaterial vulkanResources renderTarget (undefined :: Proxy Word32)
    [] pipelineDefaults { depthTestEnable = False } vertShader fragShader [globalDescriptorSet, materialDescriptorSet] Nothing
  where
  vertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
$header

layout (location = 0) out vec2 texCoordsVarying;

void main()
{
    texCoordsVarying = vec2(gl_VertexIndex & 2, (gl_VertexIndex << 1) & 2 );
    gl_Position = vec4(texCoordsVarying * 2.0f + -1.0f, 1.0f, 1.0f);
}

|])
  fragShader = $(compileShaderQ Nothing "frag" Nothing [qm|
$header

layout (location = 0) in vec2 texCoordsVarying;
layout (location = 0) out vec4 outColor;

layout (set = 1, binding = 0) uniform usampler2D textureSampler;

void main()
{
  uint center = texture(textureSampler, texCoordsVarying).r;

  float count = 0;
  count += float(textureOffset(textureSampler, texCoordsVarying, ivec2(-2, -2)).r != center);
  count += float(textureOffset(textureSampler, texCoordsVarying, ivec2(-2,  0)).r != center);
  count += float(textureOffset(textureSampler, texCoordsVarying, ivec2(-2,  2)).r != center);
  count += float(textureOffset(textureSampler, texCoordsVarying, ivec2( 0,  2)).r != center);
  count += float(textureOffset(textureSampler, texCoordsVarying, ivec2( 2,  2)).r != center);
  count += float(textureOffset(textureSampler, texCoordsVarying, ivec2( 2,  0)).r != center);
  count += float(textureOffset(textureSampler, texCoordsVarying, ivec2( 2, -2)).r != center);
  count += float(textureOffset(textureSampler, texCoordsVarying, ivec2( 0, -2)).r != center);

  vec3 col = vec3(1.0, 0.5, 0.0);
  outColor = mix(vec4(col, 0), vec4(col, 1), smoothstep(0, 0.5, count/8));
}
|])
