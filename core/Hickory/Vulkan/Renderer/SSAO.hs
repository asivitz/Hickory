{-# LANGUAGE PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, DerivingStrategies #-}
{-# LANGUAGE DataKinds, DeriveGeneric, DeriveAnyClass, OverloadedLists, OverloadedLabels #-}

module Hickory.Vulkan.Renderer.SSAO where

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
  , CullModeFlagBits (..), Extent2D, ImageUsageFlagBits (..), ImageAspectFlagBits (..), ImageViewType (..)
  )
import Vulkan.Zero
import Acquire (Acquire)
import Data.Generics.Labels ()
import Data.Bits ((.|.))
import Hickory.Vulkan.Types
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (compileShaderQ)
import Data.String.QM (qm)
import Hickory.Vulkan.Material (pipelineDefaults, defaultBlend, withMaterial)
import Hickory.Vulkan.Renderer.ShaderDefinitions
import Hickory.Vulkan.Framing (FramedResource)
import Hickory.Vulkan.Renderer.Types (SSAOSettings)
import Hickory.Vulkan.Textures (withIntermediateImage)

hdrFormat :: Format
hdrFormat = FORMAT_R16_SFLOAT

withSSAOViewableImage :: VulkanResources -> Extent2D -> Acquire ViewableImage
withSSAOViewableImage vulkanResources@VulkanResources { deviceContext = deviceContext } extent = do
  hdrImageRaw  <- withIntermediateImage vulkanResources hdrFormat (IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. IMAGE_USAGE_INPUT_ATTACHMENT_BIT) extent SAMPLE_COUNT_1_BIT
  hdrImageView <- with2DImageView deviceContext hdrFormat IMAGE_ASPECT_COLOR_BIT hdrImageRaw IMAGE_VIEW_TYPE_2D 0 1
  pure $ ViewableImage hdrImageRaw hdrImageView hdrFormat

withSSAORenderConfig :: VulkanResources -> Swapchain -> Acquire RenderConfig
withSSAORenderConfig VulkanResources { deviceContext = DeviceContext{..} } Swapchain {..} = do
  renderPass <- withRenderPass device zero
    { attachments  = [hdrAttachmentDescription]
    , subpasses    = [subpass]
    , dependencies = [dependency]
    } Nothing mkAcquire

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
  dependency :: SubpassDependency
  dependency = zero
    { srcSubpass    = SUBPASS_EXTERNAL
    , dstSubpass    = 0
    , srcStageMask  = PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT .|. PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
    , srcAccessMask = ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
    , dstStageMask  = PIPELINE_STAGE_FRAGMENT_SHADER_BIT
    , dstAccessMask = ACCESS_SHADER_READ_BIT
    }

withSSAOMaterial :: VulkanResources -> RenderConfig -> FramedResource PointedDescriptorSet -> FramedResource PointedDescriptorSet -> Acquire (Material SSAOSettings)
withSSAOMaterial vulkanResources renderConfig globalDescriptorSet materialDescriptorSet =
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

layout (push_constant) uniform constants { uint kernelSize; float kernelRadius; } PushConstants;

layout (location = 0) in vec2 inTexCoords;
layout (location = 1) in vec3 inViewRay;
layout (location = 2) in vec3 inWorldRay;

layout (location = 0) out vec4 outColor;

layout (set = 0, binding = 4) uniform sampler2DArrayShadow shadowMap;
layout (set = 1, binding = 0) uniform sampler2D gbuffer[4];
layout (set = 1, binding = 2) uniform sampler2D ssaoNoise;

layout (scalar, set = 1, binding = 1) uniform SSAOKernel { vec3 kernel[$maxSSAOKernelSizeString]; } ssaoKernel;

float bias = 0.025;

float linearizeDepth(float depth, float nearPlane, float farPlane)
{
    return nearPlane * farPlane / (farPlane - depth * (farPlane - nearPlane));
}

void main ()
{
  if (PushConstants.kernelSize <= 0)
  {
    outColor = vec4(1);
    return;
  }

  vec3 worldNormal = texture(gbuffer[1], inTexCoords).xyz;
  vec3 viewNormal = mat3(globals.viewMat) * worldNormal;
  float depth = texture(gbuffer[3], inTexCoords).r;
  depth = linearizeDepth(depth, globals.nearPlane, globals.farPlane);
  vec3 viewPos  = inViewRay * (depth / globals.farPlane);

	ivec2 texSize = textureSize(gbuffer[1], 0);
	ivec2 noiseSize = textureSize(ssaoNoise, 0);
	vec2 noiseCoord = vec2(float(texSize.x)/float(noiseSize.x), float(texSize.y)/(noiseSize.y))
                  * inTexCoords;
	vec3 noise = texture(ssaoNoise, noiseCoord).xyz;

	vec3 tangent = normalize(noise - viewNormal * dot(noise, viewNormal));
	vec3 bitangent = cross(tangent, viewNormal);
	mat3 TBN = mat3(tangent, bitangent, viewNormal);

	float occlusion = 0;
	for(int i = 0; i < PushConstants.kernelSize; i++)
	{
  	vec3 samplePos = TBN * ssaoKernel.kernel[i].xyz;
  	samplePos = viewPos + samplePos * PushConstants.kernelRadius;

 	  vec4 sampleOffset = vec4(samplePos, 1.0);
		sampleOffset = globals.projMat * sampleOffset;
		sampleOffset.xyz = (sampleOffset.xyz / sampleOffset.w) * 0.5 + 0.5; // Perspective divide and move to texture coord space

		float sampleDepth = texture(gbuffer[3], sampleOffset.xy).r;
    sampleDepth = linearizeDepth(sampleDepth, globals.nearPlane, globals.farPlane);

		float inRange = smoothstep(0.0, 1.0, PushConstants.kernelRadius / abs(viewPos.z - sampleDepth));
  	occlusion += (sampleDepth <= samplePos.z - bias ? 1 : 0)
               * inRange;
	}
	occlusion = 1.0 - (occlusion / float(PushConstants.kernelSize));

	outColor = vec4(occlusion);
}
|])
