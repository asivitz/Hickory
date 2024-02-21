{-# LANGUAGE PatternSynonyms, DuplicateRecordFields, OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, DerivingStrategies #-}
{-# LANGUAGE DataKinds, DeriveGeneric, DeriveAnyClass, OverloadedLists, OverloadedLabels #-}

module Hickory.Vulkan.Forward.GBuffer where

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
  , ImageUsageFlagBits(..)
  , Filter (..), SamplerAddressMode (..), PrimitiveTopology (..), DescriptorSetLayout, Framebuffer, Extent2D, SamplerMipmapMode (..)
  )
import Vulkan.Zero
import Acquire.Acquire (Acquire)
import Data.Generics.Labels ()
import Hickory.Vulkan.Textures (withIntermediateImage, withImageSampler)
import Data.Bits ((.|.))
import Hickory.Vulkan.Types
import Hickory.Vulkan.RenderPass (createFramebuffer)
import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (compileShaderQ)
import Data.String.QM (qm)
import Hickory.Vulkan.Monad (BufferedUniformMaterial, withBufferedUniformMaterial)
import Hickory.Vulkan.Material (pipelineDefaults, PipelineOptions(..), defaultBlend)
import Hickory.Vulkan.Forward.Types (StaticConstants, AnimatedConstants, GBufferPushConsts)
import Hickory.Vulkan.Forward.ShaderDefinitions
import Hickory.Vulkan.Framing (FramedResource)
import Data.Word (Word32)

depthFormat :: Format
depthFormat = FORMAT_D32_SFLOAT

hdrFormat :: Format
hdrFormat = FORMAT_R16G16B16A16_SFLOAT

normalFormat :: Format
normalFormat = FORMAT_R16G16B16A16_SFLOAT

objIDFormat :: Format
objIDFormat = FORMAT_R16_UINT

withDepthViewableImage :: VulkanResources -> Extent2D -> Acquire ViewableImage
withDepthViewableImage vulkanResources extent = do
  depthImageRaw  <- withDepthImage vulkanResources extent depthFormat SAMPLE_COUNT_1_BIT IMAGE_USAGE_SAMPLED_BIT 1
  depthImageView <- with2DImageView vulkanResources.deviceContext depthFormat IMAGE_ASPECT_DEPTH_BIT depthImageRaw 0 1
  pure $ ViewableImage depthImageRaw depthImageView depthFormat

withGBufferFrameBuffer :: VulkanResources -> RenderConfig -> ViewableImage -> Acquire (Framebuffer, [DescriptorSpec])
withGBufferFrameBuffer vulkanResources@VulkanResources { deviceContext = deviceContext@DeviceContext{..} } RenderConfig {..} depthImage = do
  sampler <- withImageSampler vulkanResources FILTER_LINEAR SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE SAMPLER_MIPMAP_MODE_LINEAR

  -- Target textures for the gbuffer pass

  albedoImageRaw  <- withIntermediateImage vulkanResources hdrFormat (IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. IMAGE_USAGE_INPUT_ATTACHMENT_BIT) extent SAMPLE_COUNT_1_BIT
  albedoImageView <- with2DImageView deviceContext hdrFormat IMAGE_ASPECT_COLOR_BIT albedoImageRaw 0 1
  let albedoImage = ViewableImage albedoImageRaw albedoImageView hdrFormat

  normalImageRaw  <- withIntermediateImage vulkanResources normalFormat (IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. IMAGE_USAGE_INPUT_ATTACHMENT_BIT) extent SAMPLE_COUNT_1_BIT
  normalImageView <- with2DImageView deviceContext normalFormat IMAGE_ASPECT_COLOR_BIT normalImageRaw 0 1
  let normalImage = ViewableImage normalImageRaw normalImageView normalFormat

  objIDImageRaw  <- withIntermediateImage vulkanResources objIDFormat (IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. IMAGE_USAGE_TRANSFER_SRC_BIT) extent SAMPLE_COUNT_1_BIT
  objIDImageView <- with2DImageView deviceContext objIDFormat IMAGE_ASPECT_COLOR_BIT objIDImageRaw 0 1
  let objIDImage = ViewableImage objIDImageRaw objIDImageView objIDFormat

  let ViewableImage _ depthImageView _ = depthImage

  let descriptorSpecs = [ ImageDescriptor [(albedoImage,sampler)]
                        , ImageDescriptor [(normalImage,sampler)]
                        , ImageDescriptor [(objIDImage,sampler)]
                        , ImageDescriptor [(depthImage,sampler)]
                        ]
  (,descriptorSpecs) <$> createFramebuffer device renderPass extent [albedoImageView, normalImageView, objIDImageView, depthImageView]

withGBufferRenderConfig :: VulkanResources -> Swapchain -> Acquire RenderConfig
withGBufferRenderConfig VulkanResources { deviceContext = DeviceContext{..} } Swapchain {..} = do
  renderPass <- withRenderPass device zero
    { attachments  = [albedoAttachmentDescription, normalAttachmentDescription, objIDAttachmentDescription, depthAttachmentDescription ]
    , subpasses    = [gbufferSubpass]
    , dependencies = [gbufferDependency]
    } Nothing mkAcquire

  let cullModeOverride = Nothing
      samples = SAMPLE_COUNT_1_BIT
  pure RenderConfig {..}
  where
  albedoAttachmentDescription :: AttachmentDescription
  albedoAttachmentDescription = zero
    { format         = hdrFormat
    , samples        = SAMPLE_COUNT_1_BIT
    , loadOp         = ATTACHMENT_LOAD_OP_CLEAR
    , storeOp        = ATTACHMENT_STORE_OP_STORE
    , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
    , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
    , initialLayout  = IMAGE_LAYOUT_UNDEFINED
    , finalLayout    = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
    }
  normalAttachmentDescription :: AttachmentDescription
  normalAttachmentDescription = zero
    { format         = hdrFormat
    , samples        = SAMPLE_COUNT_1_BIT
    , loadOp         = ATTACHMENT_LOAD_OP_CLEAR
    , storeOp        = ATTACHMENT_STORE_OP_STORE
    , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
    , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
    , initialLayout  = IMAGE_LAYOUT_UNDEFINED
    , finalLayout    = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
    }
  depthAttachmentDescription :: AttachmentDescription
  depthAttachmentDescription = zero
    { format         = depthFormat
    , samples        = SAMPLE_COUNT_1_BIT
    , loadOp         = ATTACHMENT_LOAD_OP_CLEAR
    , storeOp        = ATTACHMENT_STORE_OP_STORE
    , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
    , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
    , initialLayout  = IMAGE_LAYOUT_UNDEFINED
    , finalLayout    = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
    }
  objIDAttachmentDescription :: AttachmentDescription
  objIDAttachmentDescription = zero
    { format         = objIDFormat
    , samples        = SAMPLE_COUNT_1_BIT
    , loadOp         = ATTACHMENT_LOAD_OP_CLEAR
    , storeOp        = ATTACHMENT_STORE_OP_STORE
    , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
    , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
    , initialLayout  = IMAGE_LAYOUT_UNDEFINED
    , finalLayout    = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
    }
  gbufferSubpass :: SubpassDescription
  gbufferSubpass = zero
    { pipelineBindPoint = PIPELINE_BIND_POINT_GRAPHICS
    , colorAttachments =
      [ zero
        { attachment = 0
        , layout     = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        }
      , zero
        { attachment = 1
        , layout     = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        }
      , zero
        { attachment = 2
        , layout     = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        }
      ]
    , depthStencilAttachment = Just $ zero
      { attachment = 3
      , layout     = IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
      }
    }
  gbufferDependency :: SubpassDependency
  gbufferDependency = zero
    { srcSubpass    = SUBPASS_EXTERNAL
    , dstSubpass    = 0
    , srcStageMask  = PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT .|. PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
    , srcAccessMask = ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
    , dstStageMask  = PIPELINE_STAGE_FRAGMENT_SHADER_BIT
    , dstAccessMask = ACCESS_SHADER_READ_BIT
    }

{-
withStaticGBufferMaterial :: VulkanResources -> RenderConfig -> FramedResource PointedDescriptorSet -> DescriptorSetLayout -> Acquire (BufferedUniformMaterial GBufferPushConsts StaticConstants)
withStaticGBufferMaterial vulkanResources renderConfig globalPDS perDrawLayout
  = withBufferedUniformMaterial vulkanResources renderConfig [Position, Normal, TextureCoord] pipelineDefaults staticGBufferVertShader staticGBufferFragShader globalPDS (Just perDrawLayout)

withAnimatedGBufferMaterial :: VulkanResources -> RenderConfig -> FramedResource PointedDescriptorSet -> DescriptorSetLayout -> Acquire (BufferedUniformMaterial GBufferPushConsts AnimatedConstants)
withAnimatedGBufferMaterial vulkanResources renderConfig globalPDS perDrawLayout
  = withBufferedUniformMaterial vulkanResources renderConfig [Position, Normal, TextureCoord, JointIndices, JointWeights] pipelineDefaults animatedGBufferVertShader animatedGBufferFragShader globalPDS (Just perDrawLayout)
  -}

staticGBufferVertShader :: ByteString
staticGBufferVertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
$header
$worldGlobalsDef
$gbufferPushConstantsDef
$staticUniformsDef

layout(location = 0) in vec3 inPosition;
layout(location = 2) in vec3 inNormal;
layout(location = 3) in vec2 inTexCoord;

layout(location = 0) out vec2 texCoord;
layout(location = 1) out vec3 normal;

void main() {
  vec4 worldPosition = uniforms.modelMat * vec4(inPosition, 1.0);
  vec3 worldNormal = normalize(uniforms.normalMat * inNormal);

  gl_Position = globals.viewProjMat
              * worldPosition;

  texCoord = inTexCoord * uniforms.tiling;
  normal   = worldNormal;
}

|])

staticGBufferShadowVertShader :: ByteString
staticGBufferShadowVertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
$header
$worldGlobalsDef
$shadowPassGlobalsDef
$shadowPushConstantsDef
$staticUniformsDef

layout(location = 0) in vec3 inPosition;
layout(location = 2) in vec3 inNormal;
layout(location = 3) in vec2 inTexCoord;

layout(location = 0) out vec2 texCoord;
layout(location = 1) out vec3 normal;

void main() {
  vec4 worldPosition = uniforms.modelMat * vec4(inPosition, 1.0);
  vec3 worldNormal = normalize(uniforms.normalMat * inNormal);

  gl_Position = shadowGlobals.viewProjMat[PushConstants.cascadeIndex]
              * worldPosition;

  texCoord = inTexCoord * uniforms.tiling;
  normal   = worldNormal;
}
|])

staticGBufferFragShader :: ByteString
staticGBufferFragShader = $(compileShaderQ Nothing "frag" Nothing [qm|
$header
$worldGlobalsDef
$gbufferPushConstantsDef
$staticUniformsDef

layout(location = 0) in vec2 inTexCoord;
layout(location = 1) in vec3 inNormal;
layout (set = 2, binding = 0) uniform sampler2D texSampler;

layout(location = 0) out vec4 outAlbedo;
layout(location = 1) out vec4 outNormal;
layout(location = 2) out uint outObjectID;

void main() {
  vec4 texColor = texture(texSampler, inTexCoord);
  vec4 surfaceColor = uniforms.color;

  outAlbedo   = vec4(texColor.rgb * uniforms.color.rgb, 1);
  outNormal   = vec4(inNormal,1);
  outObjectID = PushConstants.objectID;
}
|])

animatedGBufferVertShader :: ByteString
animatedGBufferVertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
$header
$worldGlobalsDef
$gbufferPushConstantsDef
$animatedUniformsDef

layout(location = 0) in vec3 inPosition;
layout(location = 2) in vec3 inNormal;
layout(location = 3) in vec2 inTexCoord;
layout(location = 6) in vec4 inJointIndices;
layout(location = 7) in vec4 inJointWeights;

layout(location = 0) out vec2 texCoord;
layout(location = 1) out vec3 normal;

void main() {
  mat4 skinMat
    = inJointWeights.x * uniforms.boneMat[int(inJointIndices.x)]
    + inJointWeights.y * uniforms.boneMat[int(inJointIndices.y)]
    + inJointWeights.z * uniforms.boneMat[int(inJointIndices.z)]
    + inJointWeights.w * uniforms.boneMat[int(inJointIndices.w)];

  vec4 modelPos = skinMat * vec4(inPosition,1.0);
  vec4 worldPosition = uniforms.modelMat * modelPos;
  vec3 worldNormal = normalize(uniforms.normalMat * inNormal);

  gl_Position = globals.viewProjMat
              * worldPosition;

  texCoord = inTexCoord;
  normal   = worldNormal;
}
|])

animatedGBufferShadowVertShader :: ByteString
animatedGBufferShadowVertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
$header
$shadowPassGlobalsDef
$shadowPushConstantsDef
$animatedUniformsDef

layout(location = 0) in vec3 inPosition;
layout(location = 2) in vec3 inNormal;
layout(location = 3) in vec2 inTexCoord;
layout(location = 6) in vec4 inJointIndices;
layout(location = 7) in vec4 inJointWeights;

layout(location = 0) out vec2 texCoord;
layout(location = 1) out vec3 normal;

void main() {
  mat4 skinMat
    = inJointWeights.x * uniforms.boneMat[int(inJointIndices.x)]
    + inJointWeights.y * uniforms.boneMat[int(inJointIndices.y)]
    + inJointWeights.z * uniforms.boneMat[int(inJointIndices.z)]
    + inJointWeights.w * uniforms.boneMat[int(inJointIndices.w)];

  vec4 modelPos = skinMat * vec4(inPosition,1.0);
  vec4 worldPosition = uniforms.modelMat * modelPos;
  vec3 worldNormal = normalize(uniforms.normalMat * inNormal);

  gl_Position = shadowGlobals.viewProjMat[PushConstants.cascadeIndex]
              * worldPosition;

  texCoord = inTexCoord;
  normal   = worldNormal;
}
|])

animatedGBufferFragShader :: ByteString
animatedGBufferFragShader = $(compileShaderQ Nothing "frag" Nothing [qm|
$header
$worldGlobalsDef
$gbufferPushConstantsDef
$animatedUniformsDef

layout(location = 0) in vec2 inTexCoord;
layout(location = 1) in vec3 inNormal;
layout (set = 2, binding = 0) uniform sampler2D texSampler;

layout(location = 0) out vec4 outAlbedo;
layout(location = 1) out vec4 outNormal;
layout(location = 2) out uint outObjectID;

void main() {
  vec4 texColor = texture(texSampler, inTexCoord);
  vec4 surfaceColor = uniforms.color;

  outAlbedo   = vec4(texColor.rgb * uniforms.color.rgb, 1);
  outNormal   = vec4(inNormal,1);
  outObjectID = PushConstants.objectID;
}
|])

-- withStaticUnlitMaterial :: VulkanResources -> RenderConfig -> FramedResource PointedDescriptorSet -> DescriptorSetLayout -> Acquire (BufferedUniformMaterial Word32 StaticConstants)
-- withStaticUnlitMaterial vulkanResources renderConfig globalPDS perDrawLayout
--   = withBufferedUniformMaterial vulkanResources renderConfig [Position, TextureCoord] pipelineDefaults staticUnlitVertShader unlitFragShader globalPDS (Just perDrawLayout)

simpleFragShader :: ByteString
simpleFragShader = $(compileShaderQ Nothing "frag" Nothing [qm|
$fragHeader
$pushConstantsDef
$staticUniformsDef

void main() {
  outColor = uniforms.color;
}

|])

withLineMaterial :: VulkanResources -> RenderConfig -> FramedResource PointedDescriptorSet -> Acquire (BufferedUniformMaterial Word32 StaticConstants)
withLineMaterial vulkanResources renderConfig globalPDS = withBufferedUniformMaterial vulkanResources renderConfig [Position] pipelineOptions vertShader simpleFragShader globalPDS Nothing
  where
  pipelineOptions = (pipelineDefaults [defaultBlend]) { primitiveTopology = PRIMITIVE_TOPOLOGY_LINE_LIST, depthTestEnable = False }
  vertShader :: ByteString
  vertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
  $header
  $worldGlobalsDef
  $pushConstantsDef
  $staticUniformsDef

  layout(location = 0) in vec3 inPosition;

  void main() {
      gl_Position = globals.viewProjMat
                  * uniforms.modelMat
                  * vec4(inPosition, 1.0);
  }

  |])

withPointMaterial :: VulkanResources -> RenderConfig -> FramedResource PointedDescriptorSet -> Acquire (BufferedUniformMaterial Word32 StaticConstants)
withPointMaterial vulkanResources renderConfig globalPDS = withBufferedUniformMaterial vulkanResources renderConfig [Position] pipelineOptions vertShader simpleFragShader globalPDS Nothing
  where
  pipelineOptions = (pipelineDefaults [defaultBlend]) { primitiveTopology = PRIMITIVE_TOPOLOGY_POINT_LIST, depthTestEnable = False }
  vertShader :: ByteString
  vertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
  $header
  $worldGlobalsDef
  $pushConstantsDef
  $staticUniformsDef

  layout(location = 0) in vec3 inPosition;

  void main() {
      gl_PointSize = 20;
      gl_Position = globals.viewProjMat
                  * uniforms.modelMat
                  * vec4(inPosition, 1.0);
  }

  |])

staticUnlitVertShader :: ByteString
staticUnlitVertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
$header
$worldGlobalsDef
$pushConstantsDef
$staticUniformsDef

layout(location = 0) in vec3 inPosition;
layout(location = 3) in vec2 inTexCoord;
layout(location = 1) out vec2 texCoord;


void main() {
  texCoord = uniforms.tiling * inTexCoord;
  gl_Position = globals.viewProjMat
              * uniforms.modelMat
              * vec4(inPosition, 1.0);
}

|])

overlayVertShader :: ByteString
overlayVertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
$header
$overlayGlobalsDef
$pushConstantsDef
$staticUniformsDef

layout(location = 0) in vec3 inPosition;
layout(location = 3) in vec2 inTexCoord;
layout(location = 1) out vec2 texCoord;


void main() {
  texCoord = uniforms.tiling * inTexCoord;
  gl_Position = globals.viewProjMat
              * uniforms.modelMat
              * vec4(inPosition, 1.0);
}

|])

unlitFragShader :: ByteString
unlitFragShader = $(compileShaderQ Nothing "frag" Nothing [qm|
$fragHeader
$pushConstantsDef
$staticUniformsDef

layout(location = 1) in vec2 texCoord;

layout (set = 2, binding = 0) uniform sampler2D texSampler;

void main() {
  vec4 texColor = texture(texSampler, texCoord);
  outColor = uniforms.color * texColor;
}

|])

-- withOverlayMaterial :: VulkanResources -> RenderConfig -> FramedResource PointedDescriptorSet -> DescriptorSetLayout -> Acquire (BufferedUniformMaterial Word32 StaticConstants)
-- withOverlayMaterial vulkanResources renderConfig globalPDS perDrawLayout
--   = withBufferedUniformMaterial vulkanResources renderConfig [Position, TextureCoord] pipelineDefaults overlayVertShader unlitFragShader globalPDS (Just perDrawLayout)
