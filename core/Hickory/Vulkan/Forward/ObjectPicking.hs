{-# LANGUAGE DataKinds, PatternSynonyms, QuasiQuotes, TemplateHaskell  #-}
{-# LANGUAGE DuplicateRecordFields, OverloadedRecordDot #-}
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
  , Filter (..), SamplerAddressMode (..), CullModeFlagBits (..), ImageUsageFlagBits (..), Framebuffer
  )
import Vulkan.Zero
import Acquire.Acquire (Acquire)
import Data.Generics.Labels ()
import Hickory.Vulkan.Textures (withIntermediateImage, withImageSampler)
import Data.Bits (zeroBits, Bits ((.|.)))
import Hickory.Vulkan.Material (pipelineDefaults, PipelineOptions(..), withMaterial, noBlend)
import Hickory.Vulkan.Types
import Hickory.Vulkan.RenderPass (createFramebuffer)
import Hickory.Vulkan.Monad (BufferedUniformMaterial, withBufferedUniformMaterial)
import GHC.Generics (Generic)
import Foreign.Storable.Generic (GStorable)
import Linear (M44)
import Data.ByteString (ByteString)
import Data.Word (Word32)
import Hickory.Vulkan.Framing (FramedResource)
import Data.Proxy (Proxy)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (compileShaderQ)
import Data.String.QM (qm)
import Hickory.Vulkan.Forward.ShaderDefinitions

withObjectIDFrameBuffer :: VulkanResources -> RenderConfig -> Acquire (Framebuffer, [DescriptorSpec])
withObjectIDFrameBuffer vulkanResources@VulkanResources { deviceContext = deviceContext@DeviceContext{..} } RenderConfig {..} = do
  sampler <- withImageSampler vulkanResources FILTER_NEAREST SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE

  depthImageRaw  <- withDepthImage vulkanResources extent depthFormat samples zeroBits 1
  depthImageView <- with2DImageView deviceContext depthFormat IMAGE_ASPECT_DEPTH_BIT depthImageRaw 0 1

  objIDImageRaw  <- withIntermediateImage vulkanResources objIDFormat (IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. IMAGE_USAGE_TRANSFER_SRC_BIT) extent samples
  objIDImageView <- with2DImageView deviceContext objIDFormat IMAGE_ASPECT_COLOR_BIT objIDImageRaw 0 1
  let objIDImage = ViewableImage objIDImageRaw objIDImageView objIDFormat

  let descriptorSpecs = [ ImageDescriptor [(objIDImage,sampler)]
                        ]
  (,descriptorSpecs) <$> createFramebuffer device renderPass extent [depthImageView, objIDImageView]

objIDFormat :: Format
objIDFormat = FORMAT_R16_UINT
depthFormat :: Format
depthFormat = FORMAT_D16_UNORM


-- For e.g. mouse picking objects in scene
withObjectIDRenderConfig :: VulkanResources -> Swapchain -> Acquire RenderConfig
withObjectIDRenderConfig vulkanResources@VulkanResources { deviceContext = deviceContext@DeviceContext{..} } Swapchain {..} = do
  renderPass <- withRenderPass device zero
    { attachments  = [depthDescription, objIDDescription]
    , subpasses    = [subpass]
    , dependencies = [dependency]
    } Nothing mkAcquire

  let cullModeOverride = Nothing

  pure RenderConfig {..}
  where
  samples = SAMPLE_COUNT_1_BIT
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

withObjectIDMaterial :: VulkanResources -> RenderConfig -> FramedResource PointedDescriptorSet -> Acquire (BufferedUniformMaterial Word32 ObjectIDConstants)
withObjectIDMaterial vulkanResources renderTarget globalDS
  = withBufferedUniformMaterial vulkanResources renderTarget [Position] (pipelineDefaults [noBlend]) objectIDVertShader objectIDFragShader globalDS Nothing

objectIDVertShader :: ByteString
objectIDVertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
$vertHeader
$worldGlobalsDef
$objectIDUniformsDef

void main() { $staticVertCalc }

|])

staticObjectIDVertShader :: ByteString
staticObjectIDVertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
$vertHeader
$worldGlobalsDef
$pushConstantsDef
$staticUniformsDef

void main() { $staticVertCalc }

|])

staticObjectIDFragShader :: ByteString
staticObjectIDFragShader = $(compileShaderQ Nothing "frag" Nothing [qm|
$header
$pushConstantsDef
$staticUniformsDef

layout(location = 0) out uint outColor;

void main() {
  outColor = uniforms.objectID;
}
|])

animatedObjectIDVertShader :: ByteString
animatedObjectIDVertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
$vertHeader
$worldGlobalsDef
$pushConstantsDef
$animatedUniformsDef

layout(location = 2) in vec3 inNormal;
layout(location = 3) in vec2 inTexCoord;
layout(location = 6) in vec4 inJointIndices;
layout(location = 7) in vec4 inJointWeights;

void main()
{
    mat4 skinMat
      = inJointWeights.x * uniforms.boneMat[int(inJointIndices.x)]
      + inJointWeights.y * uniforms.boneMat[int(inJointIndices.y)]
      + inJointWeights.z * uniforms.boneMat[int(inJointIndices.z)]
      + inJointWeights.w * uniforms.boneMat[int(inJointIndices.w)];

    vec4 modelPos = skinMat * vec4(inPosition,1.0);
    vec4 worldPosition = uniforms.modelMat * modelPos;

    gl_Position = globals.viewProjMat
                * worldPosition;
}
|])

animatedObjectIDFragShader :: ByteString
animatedObjectIDFragShader = $(compileShaderQ Nothing "frag" Nothing [qm|
$header
$pushConstantsDef
$animatedUniformsDef

layout(location = 0) out uint outColor;

void main() {
  outColor = uniforms.objectID;
}
|])

objectIDFragShader :: ByteString
objectIDFragShader = $(compileShaderQ Nothing "frag" Nothing [qm|
$header
$objectIDUniformsDef

layout(location = 0) out uint outColor;

void main() {
  outColor = uniforms.objectID;
}
|])

withObjectHighlightMaterial :: VulkanResources -> RenderConfig -> FramedResource PointedDescriptorSet -> FramedResource PointedDescriptorSet -> Acquire (Material Word32)
withObjectHighlightMaterial vulkanResources renderConfig globalDescriptorSet materialDescriptorSet =
  withMaterial vulkanResources renderConfig
    [] (pipelineDefaults [noBlend]) { depthTestEnable = False } vertShader fragShader [globalDescriptorSet, materialDescriptorSet] Nothing
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
$fragHeader

layout (location = 0) in vec2 texCoordsVarying;
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
