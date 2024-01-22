{-# LANGUAGE PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, DerivingStrategies #-}
{-# LANGUAGE DataKinds, DeriveGeneric, DeriveAnyClass, OverloadedLists, OverloadedLabels #-}

module Hickory.Vulkan.Forward.Lit where

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
  , Filter (..), SamplerAddressMode (..), PrimitiveTopology (..), DescriptorSetLayout, Framebuffer
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
import Hickory.Vulkan.Material (pipelineDefaults, PipelineOptions(..))
import Hickory.Vulkan.Forward.Types (StaticConstants, AnimatedConstants)
import Hickory.Vulkan.Forward.ShaderDefinitions
import Hickory.Vulkan.Framing (FramedResource, frameResource)
import Data.Word (Word32)

depthFormat :: Format
depthFormat = FORMAT_D32_SFLOAT

resolveFormat :: Format
resolveFormat = hdrFormat

hdrFormat :: Format
hdrFormat     = FORMAT_R16G16B16A16_SFLOAT

withLitFrameBuffer :: VulkanResources -> RenderConfig -> Acquire (Framebuffer, [DescriptorSpec])
withLitFrameBuffer vulkanResources@VulkanResources { deviceContext = deviceContext@DeviceContext{..} } RenderConfig {..} = do
  sampler <- withImageSampler vulkanResources FILTER_LINEAR SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE

  -- Target textures for the lit pass
  hdrImageRaw  <- withIntermediateImage vulkanResources hdrFormat (IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. IMAGE_USAGE_INPUT_ATTACHMENT_BIT) extent maxSampleCount
  hdrImageView <- with2DImageView deviceContext hdrFormat IMAGE_ASPECT_COLOR_BIT hdrImageRaw 0 1
  let _hdrImage = ViewableImage hdrImageRaw hdrImageView hdrFormat

  depthImageRaw  <- withDepthImage vulkanResources extent depthFormat maxSampleCount IMAGE_USAGE_SAMPLED_BIT 1
  depthImageView <- with2DImageView deviceContext depthFormat IMAGE_ASPECT_DEPTH_BIT depthImageRaw 0 1
  let depthImage = ViewableImage depthImageRaw depthImageView depthFormat

  -- Target tex for the multisample resolve pass
  resolveImageRaw  <- withIntermediateImage vulkanResources resolveFormat
    (IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. IMAGE_USAGE_INPUT_ATTACHMENT_BIT)
    extent SAMPLE_COUNT_1_BIT
  resolveImageView <- with2DImageView deviceContext resolveFormat IMAGE_ASPECT_COLOR_BIT resolveImageRaw 0 1
  let resolveImage = ViewableImage resolveImageRaw resolveImageView resolveFormat
      descriptorSpecs = [ImageDescriptor [(resolveImage,sampler)], ImageDescriptor [(depthImage,sampler)]]
  (,descriptorSpecs) <$> createFramebuffer device renderPass extent [hdrImageView, depthImageView, resolveImageView]

withLitRenderConfig :: VulkanResources -> Swapchain -> Acquire RenderConfig
withLitRenderConfig vulkanResources@VulkanResources { deviceContext = deviceContext@DeviceContext{..} } Swapchain {..} = do
  renderPass <- withRenderPass device zero
    { attachments  = [hdrColorAttachmentDescription, depthAttachmentDescription, resolveAttachmentDescription]
    , subpasses    = [litSubpass]
    , dependencies = [litDependency]
    } Nothing mkAcquire

  let cullModeOverride = Nothing
      samples = maxSampleCount
  pure RenderConfig {..}
  where
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
    , storeOp        = ATTACHMENT_STORE_OP_STORE
    , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
    , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
    , initialLayout  = IMAGE_LAYOUT_UNDEFINED
    , finalLayout    = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
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

withStaticLitMaterial :: VulkanResources -> RenderConfig -> FramedResource PointedDescriptorSet -> DescriptorSetLayout -> Acquire (BufferedUniformMaterial Word32 StaticConstants)
withStaticLitMaterial vulkanResources renderConfig globalPDS perDrawLayout
  = withBufferedUniformMaterial vulkanResources renderConfig [Position, Normal, TextureCoord] pipelineDefaults staticLitVertShader staticLitFragShader globalPDS (Just perDrawLayout)

withAnimatedLitMaterial :: VulkanResources -> RenderConfig -> FramedResource PointedDescriptorSet -> DescriptorSetLayout -> Acquire (BufferedUniformMaterial Word32 AnimatedConstants)
withAnimatedLitMaterial vulkanResources renderConfig globalPDS perDrawLayout
  = withBufferedUniformMaterial vulkanResources renderConfig [Position, Normal, TextureCoord, JointIndices, JointWeights] pipelineDefaults animatedLitVertShader animatedLitFragShader globalPDS (Just perDrawLayout)

staticLitVertShader :: ByteString
staticLitVertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
$header
$worldGlobalsDef
$shadowPassGlobalsDef
$pushConstantsDef
$staticUniformsDef

layout(location = 0) in vec3 inPosition;
layout(location = 2) in vec3 inNormal;
layout(location = 3) in vec2 inTexCoord;

layout(location = 1) out vec2 texCoord;
$litVertexDef

void main() {
  vec4 worldPosition = uniforms.modelMat * vec4(inPosition, 1.0);
  vec3 worldNormal = normalize(uniforms.normalMat * inNormal);

  gl_Position = globals.viewProjMat
              * worldPosition;

  $litVertexCalc
  texCoord = inTexCoord;
}

|])

animatedLitFragShader :: ByteString
animatedLitFragShader = $(compileShaderQ Nothing "frag" Nothing [qm|
$fragHeader
$worldGlobalsDef
$pushConstantsDef
$animatedUniformsDef
$litFragDef

layout(location = 1) in vec2 texCoord;
layout (set = 2, binding = 0) uniform sampler2D texSampler;

void main() {
  vec4 texColor = texture(texSampler, texCoord);
  vec4 surfaceColor = uniforms.color;
  $litFragCalc
  outColor = vec4(litColor.rgb * texColor.rgb, litColor.a * texColor.a);
}
|])

staticLitFragShader :: ByteString
staticLitFragShader = $(compileShaderQ Nothing "frag" Nothing [qm|
$fragHeader
$worldGlobalsDef
$litFragDef
$pushConstantsDef
$staticUniformsDef

layout(location = 1) in vec2 texCoord;
layout (set = 2, binding = 0) uniform sampler2D texSampler;

void main() {
  vec4 texColor = texture(texSampler, texCoord);
  vec4 surfaceColor = uniforms.color;
  $litFragCalc
  outColor = vec4(litColor.rgb * texColor.rgb, litColor.a * texColor.a);
}
|])

animatedLitVertShader :: ByteString
animatedLitVertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
$header
$worldGlobalsDef
$shadowPassGlobalsDef
$pushConstantsDef
$animatedUniformsDef
$litVertexDef

layout(location = 0) in vec3 inPosition;
layout(location = 2) in vec3 inNormal;
layout(location = 3) in vec2 inTexCoord;
layout(location = 6) in vec4 inJointIndices;
layout(location = 7) in vec4 inJointWeights;

layout(location = 1) out vec2 texCoord;

void main()
{
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
    $litVertexCalc
}
|])

withStaticUnlitMaterial :: VulkanResources -> RenderConfig -> FramedResource PointedDescriptorSet -> DescriptorSetLayout -> Acquire (BufferedUniformMaterial Word32 StaticConstants)
withStaticUnlitMaterial vulkanResources renderConfig globalPDS perDrawLayout
  = withBufferedUniformMaterial vulkanResources renderConfig [Position, TextureCoord] pipelineDefaults staticUnlitVertShader unlitFragShader globalPDS (Just perDrawLayout)


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
  pipelineOptions = pipelineDefaults { primitiveTopology = PRIMITIVE_TOPOLOGY_LINE_LIST, depthTestEnable = False }
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
  pipelineOptions = pipelineDefaults { primitiveTopology = PRIMITIVE_TOPOLOGY_POINT_LIST, depthTestEnable = False }
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

withOverlayMaterial :: VulkanResources -> RenderConfig -> FramedResource PointedDescriptorSet -> DescriptorSetLayout -> Acquire (BufferedUniformMaterial Word32 StaticConstants)
withOverlayMaterial vulkanResources renderConfig globalPDS perDrawLayout
  = withBufferedUniformMaterial vulkanResources renderConfig [Position, TextureCoord] pipelineDefaults overlayVertShader unlitFragShader globalPDS (Just perDrawLayout)
