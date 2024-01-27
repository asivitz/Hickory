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
  , Filter (..), SamplerAddressMode (..), PrimitiveTopology (..), DescriptorSetLayout, Framebuffer, Extent2D
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
  sampler <- withImageSampler vulkanResources FILTER_LINEAR SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE

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
  withMaterial vulkanResources renderConfig [] (pipelineDefaults [defaultBlend]) vertShader fragShader [globalDescriptorSet, materialDescriptorSet] Nothing
  where
  vertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
$header
$worldGlobalsDef

layout (location = 0) out vec2 texCoords;
layout (location = 1) out vec3 viewDir;
layout (location = 2) out vec3 worldDir;

void main()
{
    texCoords = vec2(gl_VertexIndex & 2, (gl_VertexIndex << 1) & 2 );
    gl_Position = vec4(texCoords * 2.0f + -1.0f, 1.0f, 1.0f);

    vec4 nearTopLeftView     = globals.invProjMat * vec4(-1,-1,0,1);
    vec4 nearTopRightView    = globals.invProjMat * vec4(1,-1,0,1);
    vec4 nearBottomLeftView  = globals.invProjMat * vec4(-1,1,0,1);
    vec4 nearBottomRightView = globals.invProjMat * vec4(1,1,0,1);

    vec4 farTopLeftView     = globals.invProjMat * vec4(-1,-1,1,1);
    vec4 farTopRightView    = globals.invProjMat * vec4(1,-1,1,1);
    vec4 farBottomLeftView  = globals.invProjMat * vec4(-1,1,1,1);
    vec4 farBottomRightView = globals.invProjMat * vec4(1,1,1,1);

    vec4 nearTopView    = mix(nearTopLeftView,    nearTopRightView, texCoords.x);
    vec4 nearBottomView = mix(nearBottomLeftView, nearBottomRightView, texCoords.x);
    vec4 nearView       = mix(nearTopView,        nearBottomView, texCoords.y); //Upside down?

    vec4 farTopView    = mix(farTopLeftView,    farTopRightView, texCoords.x);
    vec4 farBottomView = mix(farBottomLeftView, farBottomRightView, texCoords.x);
    vec4 farView       = mix(farTopView,        farBottomView, texCoords.y); //Upside down?
    farView.xyz /= farView.w;

    viewDir = (farView - nearView).xyz;
    worldDir = mat3(globals.invViewMat) * viewDir;
}

|])
  fragShader = $(compileShaderQ Nothing "frag" Nothing [qm|
$header
$worldGlobalsDef
$shadowPassGlobalsDef

layout (location = 0) in vec2 inTexCoords;
layout (location = 1) in vec3 inViewDir;
layout (location = 2) in vec3 inWorldDir;

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
    float z = depth * 2.0 - 1.0;
    return (2.0 * nearPlane * farPlane) / (farPlane + nearPlane - z * (farPlane - nearPlane));
}

void main()
{
  float depth = texture(depthSampler, inTexCoords).r;
  depth = linearizeDepth(depth, globals.nearPlane, globals.farPlane);
  vec3 viewDir  = normalize(inViewDir);
  vec3 worldDir = normalize(inWorldDir);
  vec3 viewPos  = viewDir * depth;
  vec3 worldPos = worldDir * depth + globals.cameraPos;

  vec3 worldNormal = texture(normalSampler, inTexCoords).xyz;
  vec3 albedo      = texture(albedoSampler, inTexCoords).xyz;
  float specularity = 8; //TODO

  uint cascadeIndex = 0;

  for (uint i = 0; i < $maxShadowCascadesString; i++) {
    if (viewPos.z < shadowGlobals.splitDepths[i]) {
      cascadeIndex = i; break;
    }
  }

  vec4 shadowCoord = biasMat * shadowGlobals.viewProjMat[cascadeIndex] * vec4(worldPos, 1);

  vec4 smTexcoord;
  smTexcoord.xyw = shadowCoord.xyz / shadowCoord.w;
  smTexcoord.z = cascadeIndex;

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
  shadow = shadow / 9.0;

  vec3 lightDirection = normalize(globals.lightDirection);
  vec3 directionToLight = -lightDirection;

  float diffuseIntensity = max(0.0, dot(worldNormal, directionToLight));

  vec3 halfAngle = normalize(directionToLight + viewDir);
  float specularIntensity = pow(max(0.0, dot(worldNormal, halfAngle)), specularity);

  vec3 light = (vec3(diffuseIntensity) + vec3(specularIntensity)) * globals.sunColor;

  outColor = vec4((shadow * light + globals.ambientColor) * albedo, 1);
}
|])
