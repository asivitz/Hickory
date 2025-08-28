{-# LANGUAGE PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, DerivingStrategies #-}
{-# LANGUAGE DataKinds, DeriveGeneric, DeriveAnyClass, OverloadedLists, OverloadedLabels #-}

module Hickory.Vulkan.Renderer.Lights where

import Hickory.Vulkan.Vulkan (mkAcquire, with2DImageView)
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
  , Filter (..), SamplerAddressMode (..), Framebuffer, Extent2D, CullModeFlagBits (..), SamplerMipmapMode (..), ImageViewType (..), bindings, withDescriptorSetLayout
  )
import Vulkan.Zero
import Acquire (Acquire)
import Data.Generics.Labels ()
import Hickory.Vulkan.Textures (withIntermediateImage, withImageSampler)
import Data.Bits ((.|.))
import Hickory.Vulkan.Types
import Hickory.Vulkan.RenderPass (createFramebuffer, renderConfigRenderPass)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (compileShaderQ)
import Data.String.QM (qm)
import Hickory.Vulkan.Material (pipelineDefaults, defaultBlend, withMaterial)
import Hickory.Vulkan.Renderer.ShaderDefinitions
import Hickory.Vulkan.Framing (FramedResource)
import Hickory.Vulkan.Renderer.Types (debugName)
import Data.Word (Word32)
import qualified Data.Vector as V
import Hickory.Vulkan.DescriptorSet (descriptorSetBindings)

hdrFormat :: Format
hdrFormat = FORMAT_R16G16B16A16_SFLOAT

withColorViewableImage :: VulkanResources -> Extent2D -> Acquire ViewableImage
withColorViewableImage vulkanResources@VulkanResources { deviceContext = deviceContext } extent = do
  hdrImageRaw  <- withIntermediateImage vulkanResources hdrFormat (IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. IMAGE_USAGE_INPUT_ATTACHMENT_BIT) extent SAMPLE_COUNT_1_BIT
  hdrImageView <- with2DImageView deviceContext hdrFormat IMAGE_ASPECT_COLOR_BIT hdrImageRaw IMAGE_VIEW_TYPE_2D 0 1
  debugName vulkanResources hdrImageRaw "HDRImage"
  debugName vulkanResources hdrImageView "HDRImageView"
  pure $ ViewableImage hdrImageRaw hdrImageView hdrFormat

withLightingFrameBuffer :: VulkanResources -> RenderConfig -> ViewableImage -> Acquire (Framebuffer, DescriptorSpec)
withLightingFrameBuffer vulkanResources@VulkanResources { deviceContext = DeviceContext{..} } rc@RenderConfig {..} colorImage = do
  sampler <- withImageSampler vulkanResources FILTER_LINEAR SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE SAMPLER_MIPMAP_MODE_LINEAR

  let ViewableImage _ colorImageView _ = colorImage

  let descriptorSpec = ImageDescriptor [(colorImage,sampler)]
  fb <- createFramebuffer device (renderConfigRenderPass rc) extent [colorImageView]
  debugName vulkanResources fb "LightingFrameBuffer"
  pure (fb, descriptorSpec)

withLightingRenderConfig :: VulkanResources -> Swapchain -> Acquire RenderConfig
withLightingRenderConfig vulkanResources@VulkanResources { deviceContext = DeviceContext{..} } Swapchain {..} = do
  renderPass <- withRenderPass device zero
    { attachments  = [hdrAttachmentDescription]
    , subpasses    = [subpass]
    , dependencies
    } Nothing mkAcquire

  debugName vulkanResources renderPass "LightingRenderPass"

  let samples = SAMPLE_COUNT_1_BIT
      renderPassInfo = Left renderPass
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
    }
  dependencies = [zero
    { srcSubpass    = SUBPASS_EXTERNAL
    , dstSubpass    = 0
    , srcStageMask  = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    , srcAccessMask = ACCESS_COLOR_ATTACHMENT_WRITE_BIT
    , dstStageMask  = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    , dstAccessMask = ACCESS_COLOR_ATTACHMENT_WRITE_BIT
    }, zero
    { srcSubpass    = 0
    , dstSubpass    = SUBPASS_EXTERNAL
    , srcStageMask  = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    , srcAccessMask = ACCESS_COLOR_ATTACHMENT_WRITE_BIT
    , dstStageMask  = PIPELINE_STAGE_FRAGMENT_SHADER_BIT
    , dstAccessMask = ACCESS_SHADER_READ_BIT
    }
    ]

withDirectionalLightMaterial :: VulkanResources -> RenderConfig -> FramedResource PointedDescriptorSet -> FramedResource PointedDescriptorSet -> Acquire (Material Word32)
withDirectionalLightMaterial vulkanResources renderConfig globalDescriptorSet materialDescriptorSet = do
  sunPDS <- withDescriptorSetLayout device zero
      { bindings = V.fromList $ descriptorSetBindings [ImageDescriptor [error "Dummy image"], ImageDescriptor [error "Dummy image"]]
      } Nothing mkAcquire

  withMaterial vulkanResources "Lights" renderConfig [] (pipelineDefaults [defaultBlend]) CULL_MODE_BACK_BIT vertShader fragShader [globalDescriptorSet, materialDescriptorSet] (Just sunPDS)
  where
  VulkanResources {..} = vulkanResources
  DeviceContext {..} = deviceContext
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
    viewRay = view.xyz / view.w; // From camera toward fragment
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

layout (set = 0, binding = 4) uniform sampler2DArrayShadow shadowMap;
layout (set = 1, binding = 0) uniform sampler2D gbuffer[4];
layout (set = 1, binding = 1) uniform sampler2D ssao;
layout (set = 2, binding = 0) uniform samplerCube envMap;
layout (set = 2, binding = 1) uniform samplerCube irradianceMap;

#define PI 3.1415926535

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
  float depth = texture(gbuffer[3], inTexCoords).r;

  if (depth + 0.0001 > 1) {
    vec4 env = texture(envMap, normalize(inWorldRay));
    outColor = vec4(env.rgb * globals.envMapStrength, 1);
    return;
  }

  depth = linearizeDepth(depth, globals.nearPlane, globals.farPlane);

  vec3 worldFragmentToCamera  = -normalize(inWorldRay); // Pointing out of surface toward camera
  vec3 viewPos  = inViewRay * (depth / globals.farPlane);
  vec3 worldPos = inWorldRay * (depth / globals.farPlane) + globals.cameraPos;

  vec3 worldNormal  = normalize(texture(gbuffer[1], inTexCoords).xyz);
  vec4 albedo       = texture(gbuffer[0], inTexCoords);
  vec4 material     = texture(gbuffer[2], inTexCoords);
  float roughness   = clamp(material.x, 0.089, 1.0); // prevent divide by zero and artifacts
  float reflectance = material.y;
  float metallic    = material.z;
  float lightScale  = material.a; // Overall multiplier of the light sources against this material

  vec3 lightDirection = normalize(globals.lightDirection);
  vec3 directionToLight = -lightDirection;

  float shadow = calcShadow(viewPos, worldPos + worldNormal * shadowGlobals.shadowBiasSlope) * mix(0.3,1,albedo.a);

  float nDotL = max(0.0, dot(worldNormal, directionToLight));
  float nDotV = max(0.0, dot(worldNormal, worldFragmentToCamera));
  vec3 halfVector = normalize(directionToLight + worldFragmentToCamera);
  float hDotV = max(0.0, dot(halfVector, worldFragmentToCamera));

  float nDotHalf = max(0.0, dot(worldNormal, halfVector));
  float roughnessSquared = roughness * roughness;

  float denominator = nDotHalf * nDotHalf * (roughnessSquared - 1) + 1;
  float ggx = roughnessSquared / (PI * denominator * denominator + 1e-5);

  float normalDistributionFunction = ggx;

  float k = (roughness + 1.0) * (roughness + 1.0) / 8.0;
  float lambdaV = nDotV / (nDotV * (1.0 - k) + k);
  float lambdaL = nDotL / (nDotL * (1.0 - k) + k);
  float smithGGX = lambdaV * lambdaL;

  float geometryFunction = smithGGX;

  float refl = 0.16 * reflectance * reflectance;
  vec3 f0 = mix(vec3(0.04), albedo.rgb, metallic);
  //vec3 f0 = mix(vec3(refl, refl, refl), albedo.rgb, metallic);
  float f = pow(clamp(1.0 - hDotV, 0.0, 1.0), 5.0);
  vec3 fresnelFunction = f0 + (1.0 - f0) * f;

  vec3 cookTorrance = normalDistributionFunction
                    * geometryFunction
                    * fresnelFunction
                    / (4 * nDotL * nDotV + 1e-5);

  vec3 diffuseColor = (1.0 - metallic) * albedo.rgb * (1.0 - fresnelFunction);
  float lambert = 1.0 / PI;

  vec3 diffuse = diffuseColor * lambert;
  vec3 specular = cookTorrance;

  vec3 surfaceColor = diffuse * globals.diffuseMask + specular * globals.specularMask;
  vec3 light = surfaceColor * globals.sunColor * lightScale * nDotL;

  float ao = texture(ssao, inTexCoords).r;

  // ambient
  vec3 ambientkS = f0 + (max(vec3(1.0 - roughness), f0) - f0) * pow(clamp(1.0 - nDotV, 0.0, 1.0), 5.0);
  vec3 ambientkD = 1.0 - ambientkS;
  vec3 irradiance = texture(irradianceMap, worldNormal).rgb;

  vec3 combined
    = mix(1, shadow, globals.shadowsMask) * light
    + mix(1, ao, globals.ssaoMask) * albedo.rgb * globals.irradianceStrength * lightScale * (ambientkD * irradiance) * min(albedo.a, 1);
  outColor = vec4(combined, 1);
}
|])
