{-# LANGUAGE PatternSynonyms, DuplicateRecordFields, OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, DerivingStrategies #-}
{-# LANGUAGE DataKinds, DeriveGeneric, DeriveAnyClass, OverloadedLists, OverloadedLabels #-}

module Hickory.Vulkan.Renderer.GBuffer where

import Hickory.Vulkan.Vulkan (mkAcquire, withDepthImage, with2DImageView, with2DImageViewMips)
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
  , Filter (..), SamplerAddressMode (..), Extent2D, SamplerMipmapMode (..), ImageViewType (..)
  )
import Vulkan.Zero
import Acquire (Acquire)
import Data.Generics.Labels ()
import Hickory.Vulkan.Textures (withIntermediateImage, withTextureImage, withImageSamplerMips)
import Data.Bits ((.|.))
import Hickory.Vulkan.Types
import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (compileShaderQ)
import Data.String.QM (qm)
import Hickory.Vulkan.Renderer.ShaderDefinitions
import Hickory.Vulkan.DescriptorSet (withDescriptorSet)

depthFormat :: Format
depthFormat = FORMAT_D32_SFLOAT

hdrFormat :: Format
hdrFormat = FORMAT_R16G16B16A16_SFLOAT

normalFormat :: Format
normalFormat = FORMAT_R16G16B16A16_SFLOAT

-- roughness, reflectance, metallic
materialFormat :: Format
materialFormat = FORMAT_R16G16B16A16_SFLOAT

objIDFormat :: Format
objIDFormat = FORMAT_R16_UINT

withDepthViewableImage :: VulkanResources -> Extent2D -> Acquire ViewableImage
withDepthViewableImage vulkanResources extent = do
  depthImageRaw  <- withDepthImage vulkanResources extent depthFormat SAMPLE_COUNT_1_BIT IMAGE_USAGE_SAMPLED_BIT 1
  depthImageView <- with2DImageView vulkanResources.deviceContext depthFormat IMAGE_ASPECT_DEPTH_BIT depthImageRaw IMAGE_VIEW_TYPE_2D 0 1
  pure $ ViewableImage depthImageRaw depthImageView depthFormat

withAlbedoViewableImage :: VulkanResources -> Extent2D -> Acquire ViewableImage
withAlbedoViewableImage vulkanResources extent = do
  albedoImageRaw  <- withIntermediateImage vulkanResources hdrFormat (IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. IMAGE_USAGE_INPUT_ATTACHMENT_BIT) extent SAMPLE_COUNT_1_BIT
  albedoImageView <- with2DImageView vulkanResources.deviceContext hdrFormat IMAGE_ASPECT_COLOR_BIT albedoImageRaw IMAGE_VIEW_TYPE_2D 0 1
  pure $ ViewableImage albedoImageRaw albedoImageView hdrFormat

withNormalViewableImage :: VulkanResources -> Extent2D -> Acquire ViewableImage
withNormalViewableImage vulkanResources extent = do
  normalImageRaw  <- withIntermediateImage vulkanResources normalFormat (IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. IMAGE_USAGE_INPUT_ATTACHMENT_BIT) extent SAMPLE_COUNT_1_BIT
  normalImageView <- with2DImageView vulkanResources.deviceContext normalFormat IMAGE_ASPECT_COLOR_BIT normalImageRaw IMAGE_VIEW_TYPE_2D 0 1
  pure $ ViewableImage normalImageRaw normalImageView normalFormat

withMaterialViewableImage :: VulkanResources -> Extent2D -> Acquire ViewableImage
withMaterialViewableImage = withNormalViewableImage

withObjIDViewableImage :: VulkanResources -> Extent2D -> Acquire ViewableImage
withObjIDViewableImage vulkanResources extent = do
  objIDImageRaw  <- withIntermediateImage vulkanResources objIDFormat (IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. IMAGE_USAGE_TRANSFER_SRC_BIT) extent SAMPLE_COUNT_1_BIT
  objIDImageView <- with2DImageView vulkanResources.deviceContext objIDFormat IMAGE_ASPECT_COLOR_BIT objIDImageRaw IMAGE_VIEW_TYPE_2D 0 1
  pure $ ViewableImage objIDImageRaw objIDImageView objIDFormat

withGBufferRenderConfig :: VulkanResources -> Swapchain -> Acquire RenderConfig
withGBufferRenderConfig VulkanResources { deviceContext = DeviceContext{..} } Swapchain {..} = do
  renderPass <- withRenderPass device zero
    { attachments  = [albedoAttachmentDescription, normalAttachmentDescription, materialAttachmentDescription, objIDAttachmentDescription, depthAttachmentDescription ]
    , subpasses    = [gbufferSubpass]
    , dependencies = [gbufferDependency]
    } Nothing mkAcquire

  let cullModeOverride = Nothing
      samples = SAMPLE_COUNT_1_BIT
      renderPassInfo = Left renderPass
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
    , finalLayout    = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
    }
  normalAttachmentDescription :: AttachmentDescription
  normalAttachmentDescription = zero
    { format         = normalFormat
    , samples        = SAMPLE_COUNT_1_BIT
    , loadOp         = ATTACHMENT_LOAD_OP_CLEAR
    , storeOp        = ATTACHMENT_STORE_OP_STORE
    , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
    , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
    , initialLayout  = IMAGE_LAYOUT_UNDEFINED
    , finalLayout    = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
    }
  materialAttachmentDescription :: AttachmentDescription
  materialAttachmentDescription = normalAttachmentDescription
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
      , zero
        { attachment = 3
        , layout     = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        }
      ]
    , depthStencilAttachment = Just $ zero
      { attachment = 4
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

loadGBufTextures :: VulkanResources -> FilePath -> FilePath -> Acquire PointedDescriptorSet
loadGBufTextures vulkanResources albedo normal = do
  let opts = pngLoadOptions
      form = formatForImageType opts.fileType

  alb <- do
    (im, mipLevels) <- withTextureImage vulkanResources True False pngLoadOptions albedo
    iv <- with2DImageViewMips vulkanResources.deviceContext form IMAGE_ASPECT_COLOR_BIT im mipLevels IMAGE_VIEW_TYPE_2D 0 1
    samp <- withImageSamplerMips vulkanResources mipLevels FILTER_LINEAR SAMPLER_ADDRESS_MODE_REPEAT SAMPLER_MIPMAP_MODE_NEAREST
    pure $ ImageDescriptor [(ViewableImage im iv form, samp)]

  nor <- do
    (im, mipLevels) <- withTextureImage vulkanResources True False pngLoadOptions normal
    iv <- with2DImageViewMips vulkanResources.deviceContext form IMAGE_ASPECT_COLOR_BIT im mipLevels IMAGE_VIEW_TYPE_2D 0 1
    samp <- withImageSamplerMips vulkanResources mipLevels FILTER_LINEAR SAMPLER_ADDRESS_MODE_REPEAT SAMPLER_MIPMAP_MODE_NEAREST
    pure $ ImageDescriptor [(ViewableImage im iv form, samp)]

  withDescriptorSet vulkanResources [alb,nor]

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
layout(location = 8) in vec4 inTangent;

layout(location = 0) out vec2 texCoord;
layout(location = 1) out vec3 normal;
layout(location = 2) out vec4 color;
layout(location = 3) flat out uint objectId;
layout(location = 4) out vec4 material;
layout(location = 8) out mat3 TBN;

void main() {
  vec4 worldPosition = uniforms.modelMat * vec4(inPosition, 1.0);
  vec3 worldNormal = normalize(uniforms.normalMat * inNormal);
	vec3 worldTangent = normalize(uniforms.normalMat * inTangent.xyz);
  vec3 bitangent = -cross(inNormal.xyz, inTangent.xyz) * inTangent.w; // Why negative? No idea... But it works w/ blender exports.
  vec3 worldBitangent = normalize(uniforms.normalMat * bitangent);

  gl_Position = globals.viewProjMat
              * worldPosition;

  texCoord = inTexCoord * uniforms.tiling;
  normal   = worldNormal;
  color    = uniforms.color;
  material = uniforms.material;
  TBN = mat3(worldTangent, worldBitangent, worldNormal);
  objectId = objectIds[uniformIdx];
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

void main() {
  vec4 worldPosition = uniforms.modelMat * vec4(inPosition, 1.0);

  gl_Position = shadowGlobals.viewProjMat[PushConstants.cascadeIndex]
              * worldPosition;

  texCoord = inTexCoord * uniforms.tiling;
}
|])

staticGBufferFragShader :: ByteString
staticGBufferFragShader = $(compileShaderQ Nothing "frag" Nothing [qm|
$header
$worldGlobalsDef
$gbufferPushConstantsDef

layout(location = 0) in vec2 inTexCoord;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec4 inColor;
layout(location = 3) flat in uint objectId;
layout(location = 4) in vec4 inMaterial;
layout(location = 8) in mat3 TBN;
layout (set = 2, binding = 0) uniform sampler2D albedoSampler;
layout (set = 2, binding = 1) uniform sampler2D normalSampler;

layout(location = 0) out vec4 outAlbedo;
layout(location = 1) out vec4 outNormal;
layout(location = 2) out vec4 outMaterial;
layout(location = 3) out uint outObjectID;

void main() {
  vec4 albedoColor  = texture(albedoSampler, inTexCoord);
  vec4 normalTex    = texture(normalSampler, inTexCoord);

  vec3 normal = vec3(mix(-1,1,normalTex.r), mix(-1,1,normalTex.g), normalTex.b);

  outAlbedo   = vec4(albedoColor.rgb * inColor.rgb, 1);
  outNormal   = vec4(TBN * normal.xyz,1);
  outMaterial = inMaterial;
  outObjectID = objectId;
}
|])

animatedGBufferVertShader :: ByteString
animatedGBufferVertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
$header
$worldGlobalsDef
$gbufferPushConstantsDef
$animatedUniformsDef

layout (row_major, scalar, set = 1, binding = 3) uniform SkinBlock { mat4 boneMat [1000]; } skinBlock;

layout(location = 0) in vec3 inPosition;
layout(location = 2) in vec3 inNormal;
layout(location = 3) in vec2 inTexCoord;
layout(location = 6) in vec4 inJointIndices;
layout(location = 7) in vec4 inJointWeights;
layout(location = 8) in vec4 inTangent;

layout(location = 0) out vec2 texCoord;
layout(location = 1) out vec3 normal;
layout(location = 2) out vec4 color;
layout(location = 3) flat out uint objectId;
layout(location = 4) out vec4 material;
layout(location = 8) out mat3 TBN;

void main() {
  int boneOffset = int(uniforms.skinIdx * 66);
  mat4 skinMat
    = inJointWeights.x * skinBlock.boneMat[boneOffset + int(inJointIndices.x)]
    + inJointWeights.y * skinBlock.boneMat[boneOffset + int(inJointIndices.y)]
    + inJointWeights.z * skinBlock.boneMat[boneOffset + int(inJointIndices.z)]
    + inJointWeights.w * skinBlock.boneMat[boneOffset + int(inJointIndices.w)];

  vec4 modelPos = skinMat * vec4(inPosition,1.0);
  vec4 worldPosition = uniforms.modelMat * modelPos;
  vec3 worldNormal = normalize(uniforms.normalMat * mat3(skinMat) * inNormal);
	vec3 worldTangent = normalize(uniforms.normalMat * mat3(skinMat) * inTangent.xyz);
  vec3 bitangent = -cross(inNormal.xyz, inTangent.xyz) * inTangent.w; // Why negative? No idea... But it works w/ blender exports.
	vec3 worldBitangent = normalize(uniforms.normalMat * bitangent);

  gl_Position = globals.viewProjMat
              * worldPosition;

  texCoord = inTexCoord;
  normal   = worldNormal;
  color = uniforms.color;
  material = uniforms.material;
  TBN = mat3(worldTangent, worldBitangent, worldNormal);
  objectId = objectIds[uniformIdx];
}
|])

animatedGBufferShadowVertShader :: ByteString
animatedGBufferShadowVertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
$header
$shadowPassGlobalsDef
$shadowPushConstantsDef
$animatedUniformsDef

layout (row_major, scalar, set = 1, binding = 3) uniform SkinBlock { mat4 boneMat [1000]; } skinBlock;

layout(location = 0) in vec3 inPosition;
layout(location = 2) in vec3 inNormal;
layout(location = 3) in vec2 inTexCoord;
layout(location = 6) in vec4 inJointIndices;
layout(location = 7) in vec4 inJointWeights;

layout(location = 0) out vec2 texCoord;
layout(location = 2) out vec4 color;

void main() {
  int boneOffset = int(uniforms.skinIdx * 66);
  mat4 skinMat
    = inJointWeights.x * skinBlock.boneMat[boneOffset + int(inJointIndices.x)]
    + inJointWeights.y * skinBlock.boneMat[boneOffset + int(inJointIndices.y)]
    + inJointWeights.z * skinBlock.boneMat[boneOffset + int(inJointIndices.z)]
    + inJointWeights.w * skinBlock.boneMat[boneOffset + int(inJointIndices.w)];

  vec4 modelPos = skinMat * vec4(inPosition,1.0);
  vec4 worldPosition = uniforms.modelMat * modelPos;

  gl_Position = shadowGlobals.viewProjMat[PushConstants.cascadeIndex]
              * worldPosition;

  texCoord = inTexCoord;
}
|])

animatedGBufferFragShader :: ByteString
animatedGBufferFragShader = $(compileShaderQ Nothing "frag" Nothing [qm|
$header
$worldGlobalsDef
$gbufferPushConstantsDef

layout(location = 0) in vec2 inTexCoord;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec4 inColor;
layout(location = 3) flat in uint objectId;
layout(location = 4) in vec4 inMaterial;
layout(location = 8) in mat3 TBN;
layout (set = 2, binding = 0) uniform sampler2D albedoSampler;
layout (set = 2, binding = 1) uniform sampler2D normalSampler;

layout(location = 0) out vec4 outAlbedo;
layout(location = 1) out vec4 outNormal;
layout(location = 2) out vec4 outMaterial;
layout(location = 3) out uint outObjectID;

void main() {
  vec4 albedoColor = texture(albedoSampler, inTexCoord);
  vec4 normalTex   = texture(normalSampler, inTexCoord);

  vec3 normal = vec3(mix(-1,1,normalTex.r), mix(-1,1,normalTex.g), normalTex.b);

  outAlbedo   = vec4(albedoColor.rgb * inColor.rgb, 1);
  outNormal   = vec4(TBN * normal.xyz,1);
  outMaterial = inMaterial;
  outObjectID = objectId;
}
|])

-- withStaticUnlitMaterial :: VulkanResources -> RenderConfig -> FramedResource PointedDescriptorSet -> DescriptorSetLayout -> Acquire (BufferedUniformMaterial Word32 StaticConstants)
-- withStaticUnlitMaterial vulkanResources renderConfig globalPDS perDrawLayout
--   = withBufferedUniformMaterial vulkanResources renderConfig [Position, TextureCoord] pipelineDefaults staticUnlitVertShader unlitFragShader globalPDS (Just perDrawLayout)

-- withLineMaterial :: VulkanResources -> RenderConfig -> FramedResource PointedDescriptorSet -> Acquire (BufferedUniformMaterial Word32 StaticConstants)
-- withLineMaterial vulkanResources renderConfig globalPDS = withBufferedUniformMaterial vulkanResources renderConfig [Position] pipelineOptions vertShader simpleFragShader globalPDS Nothing
--   where
--   pipelineOptions = (pipelineDefaults [defaultBlend]) { primitiveTopology = PRIMITIVE_TOPOLOGY_LINE_LIST, depthTestEnable = False }

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
