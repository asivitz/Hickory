{-# LANGUAGE PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, DerivingStrategies #-}
{-# LANGUAGE DataKinds, DeriveGeneric, DeriveAnyClass, OverloadedLists, OverloadedLabels #-}

module Hickory.Vulkan.Forward.Lights where

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
  , Filter (..), SamplerAddressMode (..), PrimitiveTopology (..), DescriptorSetLayout, Framebuffer, Extent2D, CullModeFlagBits (..), SamplerMipmapMode (..)
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
import Hickory.Vulkan.Material (pipelineDefaults, PipelineOptions(..), defaultBlend, withMaterial)
import Hickory.Vulkan.Forward.Types (StaticConstants, AnimatedConstants, GBufferPushConsts)
import Hickory.Vulkan.Forward.ShaderDefinitions
import Hickory.Vulkan.Framing (FramedResource)
import Data.Word (Word32)
import Hickory.Vulkan.Forward.GBuffer (depthFormat)

hdrFormat :: Format
hdrFormat = FORMAT_R16G16B16A16_SFLOAT

withColorViewableImage :: VulkanResources -> Extent2D -> Acquire ViewableImage
withColorViewableImage vulkanResources@VulkanResources { deviceContext = deviceContext } extent = do
  hdrImageRaw  <- withIntermediateImage vulkanResources hdrFormat (IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. IMAGE_USAGE_INPUT_ATTACHMENT_BIT) extent SAMPLE_COUNT_1_BIT
  hdrImageView <- with2DImageView deviceContext hdrFormat IMAGE_ASPECT_COLOR_BIT hdrImageRaw 0 1
  pure $ ViewableImage hdrImageRaw hdrImageView hdrFormat

withLightingFrameBuffer :: VulkanResources -> RenderConfig -> ViewableImage -> Acquire (Framebuffer, [DescriptorSpec])
withLightingFrameBuffer vulkanResources@VulkanResources { deviceContext = deviceContext@DeviceContext{..} } RenderConfig {..} colorImage = do
  sampler <- withImageSampler vulkanResources FILTER_LINEAR SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE SAMPLER_MIPMAP_MODE_LINEAR

  let ViewableImage _ colorImageView _ = colorImage

  let descriptorSpecs = [ ImageDescriptor [(colorImage,sampler)]
                        ]
  (,descriptorSpecs) <$> createFramebuffer device renderPass extent [colorImageView]

withLightingRenderConfig :: VulkanResources -> Swapchain -> Acquire RenderConfig
withLightingRenderConfig VulkanResources { deviceContext = DeviceContext{..} } Swapchain {..} = do
  renderPass <- withRenderPass device zero
    { attachments  = [hdrAttachmentDescription]
    , subpasses    = [subpass]
    , dependencies = [dependency]
    } Nothing mkAcquire

  let cullModeOverride = Nothing
      samples = SAMPLE_COUNT_1_BIT
  pure RenderConfig {..}
  where
  hdrAttachmentDescription :: AttachmentDescription
  hdrAttachmentDescription = zero
    { format         = hdrFormat
    , samples        = SAMPLE_COUNT_1_BIT
    , loadOp         = ATTACHMENT_LOAD_OP_CLEAR
    , storeOp        = ATTACHMENT_STORE_OP_STORE
    , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
    , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
    , initialLayout  = IMAGE_LAYOUT_UNDEFINED
    , finalLayout    = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
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

withDirectionalLightMaterial :: VulkanResources -> RenderConfig -> FramedResource PointedDescriptorSet -> FramedResource PointedDescriptorSet -> Acquire (Material Word32)
withDirectionalLightMaterial vulkanResources renderConfig globalDescriptorSet materialDescriptorSet =
  withMaterial vulkanResources renderConfig [] (pipelineDefaults [defaultBlend]) CULL_MODE_BACK_BIT vertShader fragShader [globalDescriptorSet, materialDescriptorSet] Nothing
  where
  vertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
$header
$worldGlobalsDef

layout (location = 0) out vec2 texCoords;
layout (location = 1) out vec3 viewRay;
layout (location = 2) out vec3 worldRay;

void main()
{
    texCoords = vec2(gl_VertexIndex & 2, (gl_VertexIndex << 1) & 2 );
    gl_Position = vec4(texCoords * 2.0f + -1.0f, 1.0f, 1.0f);

    vec4 view = globals.invProjMat * vec4(gl_Position.x,gl_Position.y,1,1);
    viewRay = view.xyz / view.w;
    worldRay = mat3(globals.invViewMat) * viewRay;
}

|])
  fragShader = $(compileShaderQ Nothing "frag" Nothing [qm|
$header
$worldGlobalsDef
$shadowPassGlobalsDef

layout (location = 0) in vec2 inTexCoords;
layout (location = 1) in vec3 inViewRay;
layout (location = 2) in vec3 inWorldRay;

layout (location = 0) out vec4 outColor;

layout (row_major, scalar, set = 0, binding = 0) uniform PostGlobals
  { int frameNumber;
  } postGlobals;

layout( push_constant, scalar ) uniform constants
{
  float exposure;
  vec3 colorShift;
  float saturation;
  float filmGrain;
} PushConstants;

layout (set = 0, binding = 4) uniform sampler2DArrayShadow shadowMap;
layout (set = 1, binding = 0) uniform sampler2D albedoSampler;
layout (set = 1, binding = 1) uniform sampler2D normalSampler;
layout (set = 1, binding = 3) uniform sampler2D depthSampler;

const mat4 biasMat = mat4(
  0.5, 0.0, 0.0, 0.0,
  0.0, 0.5, 0.0, 0.0,
  0.0, 0.0, 1.0, 0.0,
  0.5, 0.5, 0.0, 1.0 );

float linearizeDepth(float depth, float nearPlane, float farPlane)
{
    return nearPlane * farPlane / (farPlane - depth * (farPlane - nearPlane));
}

vec4 shadowCoord(uint cascadeIndex, vec3 worldPos) {
  vec4 shadowPos = biasMat * shadowGlobals.viewProjMat[cascadeIndex] * vec4(worldPos, 1);
  vec4 smTexcoord;
  smTexcoord.xyw = shadowPos.xyz / shadowPos.w;
  smTexcoord.z = cascadeIndex;
  return smTexcoord;
}

float sampleShadow(vec4 smTexcoord) {
  float shadow = 0.0;
  shadow += textureOffset(shadowMap, smTexcoord, ivec2(-1,  1));
  shadow += textureOffset(shadowMap, smTexcoord, ivec2( 1,  1));
  shadow += textureOffset(shadowMap, smTexcoord, ivec2(-1, -1));
  shadow += textureOffset(shadowMap, smTexcoord, ivec2( 1, -1));
  shadow += textureOffset(shadowMap, smTexcoord, ivec2( 0, 0));
  shadow += textureOffset(shadowMap, smTexcoord, ivec2( 1, 0));
  shadow += textureOffset(shadowMap, smTexcoord, ivec2( -1, 0));
  shadow += textureOffset(shadowMap, smTexcoord, ivec2( 0, 1));
  shadow += textureOffset(shadowMap, smTexcoord, ivec2( 0, -1));
  return shadow / 9.0;
}

float calcShadow(vec3 viewPos, vec3 worldPos)
{
  uint cascadeIndex = 0;
  float blendFactor = 0.0;
  float overlapThreshold = $cascadeOverlapThresholdString;

  for (uint i = 0; i < $maxShadowCascadesString; i++) {
    if (viewPos.z < shadowGlobals.splitDepths[i]) {
      cascadeIndex = i;
      if (i < $maxShadowCascadesString - 1 && viewPos.z > shadowGlobals.splitDepths[i] - overlapThreshold) {
          blendFactor = (viewPos.z - (shadowGlobals.splitDepths[i] - overlapThreshold)) / overlapThreshold;
      }
      break;
    }
  }

  float shadow = sampleShadow(shadowCoord(cascadeIndex, worldPos));

  if (blendFactor > 0) {
    float shadowNext = sampleShadow(shadowCoord(cascadeIndex + 1, worldPos));
    shadow = mix(shadow, shadowNext, blendFactor);
  }
  return shadow;
}

void main()
{
  float depth = texture(depthSampler, inTexCoords).r;
  depth = linearizeDepth(depth, globals.nearPlane, globals.farPlane);
  vec3 viewDir  = normalize(inViewRay);
  vec3 worldDir = normalize(inWorldRay);
  vec3 viewPos  = inViewRay * (depth / globals.farPlane);
  vec3 worldPos = inWorldRay * (depth / globals.farPlane) + globals.cameraPos;

  vec3 worldNormal = texture(normalSampler, inTexCoords).xyz;
  vec4 albedo      = texture(albedoSampler, inTexCoords);
  float specularity = 8; //TODO

  float shadow = calcShadow(viewPos, worldPos) * mix(0.3,1,albedo.a);

  vec3 lightDirection = normalize(globals.lightDirection);
  vec3 directionToLight = -lightDirection;

  float diffuseIntensity = max(0.0, dot(worldNormal, directionToLight));

  vec3 halfAngle = normalize(directionToLight + viewDir);
  float specularIntensity = pow(max(0.0, dot(worldNormal, halfAngle)), specularity);

  vec3 light = (vec3(diffuseIntensity) + vec3(specularIntensity)) * globals.sunColor;

  outColor = vec4((shadow * light + globals.ambientColor * albedo.a) * albedo.rgb, 1);
}
|])
