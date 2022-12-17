{-# LANGUAGE PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes, DerivingStrategies #-}
{-# LANGUAGE DataKinds, DeriveGeneric, DeriveAnyClass, OverloadedLists, OverloadedLabels #-}

module Hickory.Vulkan.Forward.Lit where

import Hickory.Vulkan.Vulkan (mkAcquire, ViewableImage(..), Swapchain (..), VulkanResources (..), DeviceContext (..), withDepthImage, with2DImageView)
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
import qualified Data.Vector as V
import Data.Generics.Labels ()
import Hickory.Vulkan.Textures (withIntermediateImage, withImageSampler)
import Data.Bits ((.|.), zeroBits)
import Hickory.Vulkan.Types
import Hickory.Vulkan.RenderPass (createFramebuffer)
import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (vert, frag)
import Hickory.Vulkan.Monad (BufferedUniformMaterial, withBufferedUniformMaterial)
import Hickory.Vulkan.Material (pipelineDefaults, PipelineOptions(..))
import Hickory.Vulkan.Mesh (Attribute(..))
import Hickory.Vulkan.Forward.Types (StaticConstants, AnimatedConstants)

depthFormat :: Format
depthFormat = FORMAT_D32_SFLOAT

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
      descriptorSpecs = [ImageDescriptor [(resolveImage,sampler)]]
      cullMode = CULL_MODE_BACK_BIT
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

withStaticLitMaterial :: VulkanResources -> RenderTarget -> PointedDescriptorSet -> DescriptorSetLayout -> Acquire (BufferedUniformMaterial StaticConstants)
withStaticLitMaterial vulkanResources renderTarget globalPDS perDrawLayout
  = withBufferedUniformMaterial vulkanResources renderTarget [Position, Normal, TextureCoord] pipelineDefaults staticLitVertShader litFragShader globalPDS (Just perDrawLayout)

withAnimatedLitMaterial :: VulkanResources -> RenderTarget -> PointedDescriptorSet -> DescriptorSetLayout -> Acquire (BufferedUniformMaterial AnimatedConstants)
withAnimatedLitMaterial vulkanResources renderTarget globalPDS perDrawLayout
  = withBufferedUniformMaterial vulkanResources renderTarget [Position, Normal, TextureCoord, BoneIndex, MaterialIndex] pipelineDefaults animatedLitVertShader litFragShader globalPDS (Just perDrawLayout)

staticLitVertShader :: ByteString
staticLitVertShader = [vert|
#version 450
#extension GL_EXT_scalar_block_layout : require

layout(location = 0) in vec3 inPosition;
layout(location = 2) in vec3 inNormal;
layout(location = 3) in vec2 inTexCoord;

layout(location = 0) out vec3 light;
layout(location = 1) out vec2 texCoord;
layout(location = 2) out vec4 shadowCoord;
layout(location = 3) out vec4 surfaceColor;

struct Uniforms
{
  mat4 modelMat;
  mat3 normalMat;
  vec4 color;
  vec2 tiling;
};

layout (push_constant) uniform constants { uint uniformIdx; } PushConstants;
layout (row_major, scalar, set = 1, binding = 0) uniform UniformBlock { Uniforms uniforms [128]; } uniformBlock;
layout (row_major, scalar, set = 0, binding = 0) uniform GlobalUniform
  { mat4 viewMat;
    mat4 projMat;
    vec3 cameraPos;
    mat4 lightTransform;
    vec3 lightDirection;
    vec3 sunColor;
    vec3 ambientColor;
  } globals;

const mat4 biasMat = mat4(
  0.5, 0.0, 0.0, 0.0,
  0.0, 0.5, 0.0, 0.0,
  0.0, 0.0, 1.0, 0.0,
  0.5, 0.5, 0.0, 1.0 );

void main() {
    Uniforms uniforms = uniformBlock.uniforms[PushConstants.uniformIdx];

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
    float specularIntensity = pow(max(0.0, dot(worldNormal, halfAngle)), 8);

    light = (vec3(diffuseIntensity) + vec3(specularIntensity)) * globals.sunColor;

    surfaceColor = uniforms.color;
    texCoord = inTexCoord;
    shadowCoord = biasMat * globals.lightTransform * worldPosition;
}

|]

litFragShader :: ByteString
litFragShader = [frag|
#version 450
#extension GL_EXT_scalar_block_layout : require
// #extension GL_EXT_nonuniform_qualifier : require

layout(location = 0) in vec3 light;
layout(location = 1) in vec2 texCoord;
layout(location = 2) in vec4 shadowCoord;
layout(location = 3) in vec4 surfaceColor;

layout(location = 0) out vec4 outColor;

layout (push_constant) uniform constants { uint uniformIdx; } PushConstants;
layout (set = 0, binding = 1) uniform sampler2DShadow shadowMap;
layout (row_major, scalar, set = 0, binding = 0) uniform GlobalUniform
  { mat4 viewMat;
    mat4 projMat;
    vec3 cameraPos;
    mat4 lightTransform;
    vec3 lightDirection;
    vec3 sunColor;
    vec3 ambientColor;
  } globals;
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
|]

animatedLitVertShader :: ByteString
animatedLitVertShader = [vert|
#version 450
#extension GL_EXT_scalar_block_layout : require

layout(location = 0) in vec3 inPosition;
layout(location = 2) in vec3 inNormal;
layout(location = 3) in vec2 inTexCoord;
layout(location = 4) in float inBoneIndex;
layout(location = 5) in float inMaterialIndex;

layout(location = 0) out vec3 light;
layout(location = 1) out vec2 texCoord;
layout(location = 2) out vec4 shadowCoord;
layout(location = 3) out vec4 surfaceColor;

struct Uniforms {
  mat4 modelMat;
  mat3 normalMat;
  vec4 color;
  mat4 boneMat[32];
  vec4 colors[6];
};

layout (push_constant) uniform constants { uint uniformIdx; } PushConstants;
layout (row_major, scalar, set = 1, binding = 0) uniform UniformBlock { Uniforms uniforms [128]; } uniformBlock;
layout (row_major, scalar, set = 0, binding = 0) uniform GlobalUniform
  { mat4 viewMat;
    mat4 projMat;
    vec3 cameraPos;
    mat4 lightTransform;
    vec3 lightDirection;
    vec3 sunColor;
    vec3 ambientColor;
  } globals;

const mat4 biasMat = mat4(
  0.5, 0.0, 0.0, 0.0,
  0.0, 0.5, 0.0, 0.0,
  0.0, 0.0, 1.0, 0.0,
  0.5, 0.5, 0.0, 1.0 );

void main()
{
    Uniforms uniforms = uniformBlock.uniforms[PushConstants.uniformIdx];

    vec4 modelPos = uniforms.boneMat[int(inBoneIndex)] * vec4(inPosition,1.0);
    vec4 worldPosition = uniforms.modelMat * modelPos;

    gl_Position = globals.projMat
                * globals.viewMat
                * worldPosition;

    /* Lighting */
    vec3 lightDirection = normalize(globals.lightDirection);
    vec3 directionToLight = -lightDirection;
    vec3 viewDirection = normalize(globals.cameraPos - worldPosition.xyz);

    vec3 worldNormal = normalize(uniforms.normalMat * inNormal);

    surfaceColor = uniforms.color * uniforms.colors[int(inMaterialIndex)];
    float diffuseIntensity = max(0.0, dot(worldNormal, directionToLight));

    vec3 halfAngle = normalize(directionToLight + viewDirection);
    float specularIntensity = pow(max(0.0, dot(worldNormal, halfAngle)), 8);

    light = (vec3(diffuseIntensity) + vec3(specularIntensity)) * globals.sunColor;

    shadowCoord = biasMat * globals.lightTransform * worldPosition;
}
|]

withStaticUnlitMaterial :: VulkanResources -> RenderTarget -> PointedDescriptorSet -> DescriptorSetLayout -> Acquire (BufferedUniformMaterial StaticConstants)
withStaticUnlitMaterial vulkanResources renderTarget globalPDS perDrawLayout
  = withBufferedUniformMaterial vulkanResources renderTarget [Position, TextureCoord] pipelineDefaults staticUnlitVertShader unlitFragShader globalPDS (Just perDrawLayout)

withLineMaterial :: VulkanResources -> RenderTarget -> PointedDescriptorSet -> Acquire (BufferedUniformMaterial StaticConstants)
withLineMaterial vulkanResources renderTarget globalPDS = withBufferedUniformMaterial vulkanResources renderTarget [Position] pipelineOptions lineVertShader lineFragShader globalPDS Nothing
  where
  pipelineOptions = pipelineDefaults { primitiveTopology = PRIMITIVE_TOPOLOGY_LINE_LIST, depthTestEnable = False }
  lineVertShader :: ByteString
  lineVertShader = [vert|
  #version 450
  #extension GL_EXT_scalar_block_layout : require

  layout(location = 0) in vec3 inPosition;

  struct Uniforms
  {
    mat4 modelMat;
    mat3 normalMat;
    vec4 color;
    vec2 tiling;
  };

  layout (push_constant) uniform constants { uint uniformIdx; } PushConstants;
  layout (row_major, scalar, set = 1, binding = 0) uniform UniformBlock { Uniforms uniforms [128]; } uniformBlock;
  layout (row_major, scalar, set = 0, binding = 0) uniform GlobalUniform
    { mat4 viewMat;
      mat4 projMat;
      vec3 cameraPos;
      mat4 lightTransform;
      vec3 lightDirection;
      vec3 sunColor;
      vec3 ambientColor;
    } globals;

  void main() {
      Uniforms uniforms = uniformBlock.uniforms[PushConstants.uniformIdx];
      gl_Position = globals.projMat
                  * globals.viewMat
                  * uniforms.modelMat
                  * vec4(inPosition, 1.0);
  }

  |]

  lineFragShader :: ByteString
  lineFragShader = [frag|
  #version 450
  #extension GL_EXT_scalar_block_layout : require

  layout(location = 0) out vec4 outColor;

  struct Uniforms
  {
    mat4 modelMat;
    mat3 normalMat;
    vec4 color;
    vec2 tiling;
  };

  layout (push_constant) uniform constants { uint uniformIdx; } PushConstants;
  layout (row_major, scalar, set = 1, binding = 0) uniform UniformBlock { Uniforms uniforms [128]; } uniformBlock;

  void main() {
    Uniforms uniforms = uniformBlock.uniforms[PushConstants.uniformIdx];
    outColor = uniforms.color;
  }

  |]

staticUnlitVertShader :: ByteString
staticUnlitVertShader = [vert|
#version 450
#extension GL_EXT_scalar_block_layout : require

layout(location = 0) in vec3 inPosition;
layout(location = 3) in vec2 inTexCoord;
layout(location = 1) out vec2 texCoord;

struct Uniforms
{
  mat4 modelMat;
  mat3 normalMat;
  vec4 color;
  vec2 tiling;
};

layout (push_constant) uniform constants { uint uniformIdx; } PushConstants;
layout (row_major, scalar, set = 1, binding = 0) uniform UniformBlock { Uniforms uniforms [128]; } uniformBlock;
layout (row_major, scalar, set = 0, binding = 0) uniform GlobalUniform
  { mat4 viewMat;
    mat4 projMat;
    vec3 cameraPos;
    mat4 lightTransform;
    vec3 lightDirection;
    vec3 sunColor;
    vec3 ambientColor;
  } globals;

void main() {
    Uniforms uniforms = uniformBlock.uniforms[PushConstants.uniformIdx];
    texCoord = uniforms.tiling * inTexCoord;
    gl_Position = globals.projMat
                * globals.viewMat
                * uniforms.modelMat
                * vec4(inPosition, 1.0);
}

|]

unlitFragShader :: ByteString
unlitFragShader = [frag|
#version 450
#extension GL_EXT_scalar_block_layout : require

layout(location = 0) out vec4 outColor;
layout(location = 1) in vec2 texCoord;

struct Uniforms
{
  mat4 modelMat;
  mat3 normalMat;
  vec4 color;
  vec2 tiling;
};

layout (push_constant) uniform constants { uint uniformIdx; } PushConstants;
layout (row_major, scalar, set = 1, binding = 0) uniform UniformBlock { Uniforms uniforms [128]; } uniformBlock;
layout (set = 2, binding = 0) uniform sampler2D texSampler;

void main() {
  Uniforms uniforms = uniformBlock.uniforms[PushConstants.uniformIdx];
  vec4 texColor = texture(texSampler, texCoord);
  outColor = uniforms.color * texColor;
}

|]
