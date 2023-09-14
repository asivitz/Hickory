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
  , Filter (..), SamplerAddressMode (..), CullModeFlagBits (..), PrimitiveTopology (..), DescriptorSetLayout
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
import Hickory.Vulkan.Material (pipelineDefaults, PipelineOptions(..), withMaterial)
import Hickory.Vulkan.Forward.Types (StaticConstants, AnimatedConstants)
import Hickory.Vulkan.Forward.ShaderDefinitions
import Hickory.Vulkan.Framing (FramedResource, frameResource)

depthFormat :: Format
depthFormat = FORMAT_D32_SFLOAT

withLitRenderTarget :: VulkanResources -> Swapchain -> Acquire RenderTarget
withLitRenderTarget vulkanResources@VulkanResources { deviceContext = deviceContext@DeviceContext{..} } Swapchain {..} = do
  renderPass <- withRenderPass device zero
    { attachments  = [hdrColorAttachmentDescription, depthAttachmentDescription, resolveAttachmentDescription]
    , subpasses    = [litSubpass]
    , dependencies = [litDependency]
    } Nothing mkAcquire

  sampler <- withImageSampler vulkanResources FILTER_LINEAR SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
  frameBuffers <- frameResource do

    -- Target textures for the lit pass
    hdrImageRaw  <- withIntermediateImage vulkanResources hdrFormat (IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. IMAGE_USAGE_INPUT_ATTACHMENT_BIT) extent maxSampleCount
    hdrImageView <- with2DImageView deviceContext hdrFormat IMAGE_ASPECT_COLOR_BIT hdrImageRaw
    let _hdrImage = ViewableImage hdrImageRaw hdrImageView hdrFormat

    depthImageRaw  <- withDepthImage vulkanResources extent depthFormat maxSampleCount IMAGE_USAGE_SAMPLED_BIT
    depthImageView <- with2DImageView deviceContext depthFormat IMAGE_ASPECT_DEPTH_BIT depthImageRaw
    let depthImage = ViewableImage depthImageRaw depthImageView depthFormat

    -- Target tex for the multisample resolve pass
    resolveImageRaw  <- withIntermediateImage vulkanResources resolveFormat
      (IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. IMAGE_USAGE_INPUT_ATTACHMENT_BIT)
      extent SAMPLE_COUNT_1_BIT
    resolveImageView <- with2DImageView deviceContext resolveFormat IMAGE_ASPECT_COLOR_BIT resolveImageRaw
    let resolveImage = ViewableImage resolveImageRaw resolveImageView resolveFormat
        descriptorSpecs = [ImageDescriptor [(resolveImage,sampler)], ImageDescriptor [(depthImage,sampler)]]
    (,descriptorSpecs) <$> createFramebuffer device renderPass extent [hdrImageView, depthImageView, resolveImageView]

  let cullMode = CULL_MODE_BACK_BIT
      samples = maxSampleCount
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

withStaticLitMaterial :: VulkanResources -> RenderTarget -> FramedResource PointedDescriptorSet -> DescriptorSetLayout -> Acquire (BufferedUniformMaterial StaticConstants)
withStaticLitMaterial vulkanResources renderTarget globalPDS perDrawLayout
  = withBufferedUniformMaterial vulkanResources renderTarget [Position, Normal, TextureCoord] pipelineDefaults staticLitVertShader litFragShader globalPDS (Just perDrawLayout)

withAnimatedLitMaterial :: VulkanResources -> RenderTarget -> FramedResource PointedDescriptorSet -> DescriptorSetLayout -> Acquire (BufferedUniformMaterial AnimatedConstants)
withAnimatedLitMaterial vulkanResources renderTarget globalPDS perDrawLayout
  = withBufferedUniformMaterial vulkanResources renderTarget [Position, Normal, TextureCoord, JointIndices, JointWeights] pipelineDefaults animatedLitVertShader litFragShader globalPDS (Just perDrawLayout)

staticLitVertShader :: ByteString
staticLitVertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
$header
$worldGlobalsDef
$staticUniformsDef

layout(location = 0) in vec3 inPosition;
layout(location = 2) in vec3 inNormal;
layout(location = 3) in vec2 inTexCoord;

layout(location = 0) out vec3 light;
layout(location = 1) out vec2 texCoord;
layout(location = 2) out vec4 shadowCoord;
layout(location = 3) out vec4 surfaceColor;

const mat4 biasMat = mat4(
  0.5, 0.0, 0.0, 0.0,
  0.0, 0.5, 0.0, 0.0,
  0.0, 0.0, 1.0, 0.0,
  0.5, 0.5, 0.0, 1.0 );

void main() {

    vec4 worldPosition = uniforms.modelMat * vec4(inPosition, 1.0);

    gl_Position = globals.projMat
                * globals.viewMat
                * worldPosition;

    vec3 lightDirection = normalize(globals.lightDirection);
    vec3 directionToLight = -lightDirection;
    vec3 viewDirection = normalize(globals.cameraPos - worldPosition.xyz);

    vec3 worldNormal = normalize(uniforms.normalMat * inNormal);

    float diffuseIntensity = max(0.0, dot(worldNormal, directionToLight));


    vec3 halfAngle = normalize(directionToLight + viewDirection);
    float specularIntensity = pow(max(0.0, dot(worldNormal, halfAngle)), uniforms.specularity);

    light = (vec3(diffuseIntensity) + vec3(specularIntensity)) * globals.sunColor;

    surfaceColor = uniforms.color;
    texCoord = inTexCoord;
    shadowCoord = biasMat * globals.lightTransform * worldPosition;
}

|])

litFragShader :: ByteString
litFragShader = $(compileShaderQ Nothing "frag" Nothing [qm|
$header
$worldGlobalsDef
// #extension GL_EXT_nonuniform_qualifier : require

layout(location = 0) in vec3 light;
layout(location = 1) in vec2 texCoord;
layout(location = 2) in vec4 shadowCoord;
layout(location = 3) in vec4 surfaceColor;

layout(location = 0) out vec4 outColor;

layout (set = 0, binding = 4) uniform sampler2DShadow shadowMap;
layout (set = 2, binding = 0) uniform sampler2D texSampler;

void main() {
  vec4 texColor = texture(texSampler, texCoord);

  float shadow = 0.0;
  shadow += textureProjOffset(shadowMap, shadowCoord, ivec2(-1,  1));
  shadow += textureProjOffset(shadowMap, shadowCoord, ivec2( 1,  1));
  shadow += textureProjOffset(shadowMap, shadowCoord, ivec2(-1, -1));
  shadow += textureProjOffset(shadowMap, shadowCoord, ivec2( 1, -1));
  shadow += textureProjOffset(shadowMap, shadowCoord, ivec2( 0, 0));
  shadow += textureProjOffset(shadowMap, shadowCoord, ivec2( 1, 0));
  shadow += textureProjOffset(shadowMap, shadowCoord, ivec2( -1, 0));
  shadow += textureProjOffset(shadowMap, shadowCoord, ivec2( 0, 1));
  shadow += textureProjOffset(shadowMap, shadowCoord, ivec2( 0, -1));
  shadow = shadow / 9.0;

  outColor = vec4((shadow * light + globals.ambientColor) * texColor.rgb * surfaceColor.rgb, texColor.a * surfaceColor.a);
}
|])

animatedLitVertShader :: ByteString
animatedLitVertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
$header
$worldGlobalsDef
$animatedUniformsDef

layout(location = 0) in vec3 inPosition;
layout(location = 2) in vec3 inNormal;
layout(location = 3) in vec2 inTexCoord;
layout(location = 6) in vec4 inJointIndices;
layout(location = 7) in vec4 inJointWeights;

layout(location = 0) out vec3 light;
layout(location = 1) out vec2 texCoord;
layout(location = 2) out vec4 shadowCoord;
layout(location = 3) out vec4 surfaceColor;

const mat4 biasMat = mat4(
  0.5, 0.0, 0.0, 0.0,
  0.0, 0.5, 0.0, 0.0,
  0.0, 0.0, 1.0, 0.0,
  0.5, 0.5, 0.0, 1.0 );

void main()
{
    mat4 skinMat
      = inJointWeights.x * uniforms.boneMat[int(inJointIndices.x)]
      + inJointWeights.y * uniforms.boneMat[int(inJointIndices.y)]
      + inJointWeights.z * uniforms.boneMat[int(inJointIndices.z)]
      + inJointWeights.w * uniforms.boneMat[int(inJointIndices.w)];

    vec4 modelPos = skinMat * vec4(inPosition,1.0);
    vec4 worldPosition = uniforms.modelMat * modelPos;

    gl_Position = globals.projMat
                * globals.viewMat
                * worldPosition;

    /* Lighting */
    vec3 lightDirection = normalize(globals.lightDirection);
    vec3 directionToLight = -lightDirection;
    vec3 viewDirection = normalize(globals.cameraPos - worldPosition.xyz);

    vec3 worldNormal = normalize(uniforms.normalMat * inNormal);

    surfaceColor = uniforms.color;
    float diffuseIntensity = max(0.0, dot(worldNormal, directionToLight));

    vec3 halfAngle = normalize(directionToLight + viewDirection);
    float specularIntensity = pow(max(0.0, dot(worldNormal, halfAngle)), uniforms.specularity);

    light = (vec3(diffuseIntensity) + vec3(specularIntensity)) * globals.sunColor;

    shadowCoord = biasMat * globals.lightTransform * worldPosition;
}
|])

withStaticUnlitMaterial :: VulkanResources -> RenderTarget -> FramedResource PointedDescriptorSet -> DescriptorSetLayout -> Acquire (BufferedUniformMaterial StaticConstants)
withStaticUnlitMaterial vulkanResources renderTarget globalPDS perDrawLayout
  = withBufferedUniformMaterial vulkanResources renderTarget [Position, TextureCoord] pipelineDefaults staticUnlitVertShader unlitFragShader globalPDS (Just perDrawLayout)


simpleFragShader :: ByteString
simpleFragShader = $(compileShaderQ Nothing "frag" Nothing [qm|
$header
$staticUniformsDef

layout(location = 0) out vec4 outColor;

void main() {
  outColor = uniforms.color;
}

|])

withLineMaterial :: VulkanResources -> RenderTarget -> FramedResource PointedDescriptorSet -> Acquire (BufferedUniformMaterial StaticConstants)
withLineMaterial vulkanResources renderTarget globalPDS = withBufferedUniformMaterial vulkanResources renderTarget [Position] pipelineOptions vertShader simpleFragShader globalPDS Nothing
  where
  pipelineOptions = pipelineDefaults { primitiveTopology = PRIMITIVE_TOPOLOGY_LINE_LIST, depthTestEnable = False }
  vertShader :: ByteString
  vertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
  $header
  $worldGlobalsDef
  $staticUniformsDef

  layout(location = 0) in vec3 inPosition;

  void main() {
      gl_Position = globals.projMat
                  * globals.viewMat
                  * uniforms.modelMat
                  * vec4(inPosition, 1.0);
  }

  |])

withPointMaterial :: VulkanResources -> RenderTarget -> FramedResource PointedDescriptorSet -> Acquire (BufferedUniformMaterial StaticConstants)
withPointMaterial vulkanResources renderTarget globalPDS = withBufferedUniformMaterial vulkanResources renderTarget [Position] pipelineOptions vertShader simpleFragShader globalPDS Nothing
  where
  pipelineOptions = pipelineDefaults { primitiveTopology = PRIMITIVE_TOPOLOGY_POINT_LIST, depthTestEnable = False }
  vertShader :: ByteString
  vertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
  $header
  $worldGlobalsDef
  $staticUniformsDef

  layout(location = 0) in vec3 inPosition;

  void main() {
      gl_PointSize = 20;
      gl_Position = globals.projMat
                  * globals.viewMat
                  * uniforms.modelMat
                  * vec4(inPosition, 1.0);
  }

  |])

staticUnlitVertShader :: ByteString
staticUnlitVertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
$header
$worldGlobalsDef
$staticUniformsDef

layout(location = 0) in vec3 inPosition;
layout(location = 3) in vec2 inTexCoord;
layout(location = 1) out vec2 texCoord;


void main() {
  texCoord = uniforms.tiling * inTexCoord;
  gl_Position = globals.projMat
              * globals.viewMat
              * uniforms.modelMat
              * vec4(inPosition, 1.0);
}

|])

overlayVertShader :: ByteString
overlayVertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
$header
$overlayGlobalsDef
$staticUniformsDef

layout(location = 0) in vec3 inPosition;
layout(location = 3) in vec2 inTexCoord;
layout(location = 1) out vec2 texCoord;


void main() {
  texCoord = uniforms.tiling * inTexCoord;
  gl_Position = globals.projMat
              * globals.viewMat
              * uniforms.modelMat
              * vec4(inPosition, 1.0);
}

|])

unlitFragShader :: ByteString
unlitFragShader = $(compileShaderQ Nothing "frag" Nothing [qm|
$header
$staticUniformsDef

layout(location = 0) out vec4 outColor;
layout(location = 1) in vec2 texCoord;

layout (set = 2, binding = 0) uniform sampler2D texSampler;

void main() {
  vec4 texColor = texture(texSampler, texCoord);
  outColor = uniforms.color * texColor;
}

|])

withOverlayMaterial :: VulkanResources -> RenderTarget -> FramedResource PointedDescriptorSet -> DescriptorSetLayout -> Acquire (BufferedUniformMaterial StaticConstants)
withOverlayMaterial vulkanResources renderTarget globalPDS perDrawLayout
  = withBufferedUniformMaterial vulkanResources renderTarget [Position, TextureCoord] pipelineDefaults overlayVertShader unlitFragShader globalPDS (Just perDrawLayout)
