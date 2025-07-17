{-# LANGUAGE PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, DerivingStrategies #-}
{-# LANGUAGE DataKinds, DeriveGeneric, DeriveAnyClass, OverloadedLists, OverloadedLabels #-}

module Hickory.Vulkan.Renderer.Decals where

import Hickory.Vulkan.Vulkan (mkAcquire)
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
  )
import Vulkan.Zero
import Acquire (Acquire)
import Data.Generics.Labels ()
import Hickory.Vulkan.Types
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (compileShaderQ)
import Data.String.QM (qm)
import Hickory.Vulkan.Renderer.ShaderDefinitions
import Hickory.Vulkan.Renderer.Types (debugName)

hdrFormat :: Format
hdrFormat = FORMAT_R16G16B16A16_SFLOAT

withDecalRenderConfig :: VulkanResources -> Swapchain -> Acquire RenderConfig
withDecalRenderConfig vulkanResources@VulkanResources { deviceContext = DeviceContext{..} } Swapchain {..} = do
  renderPass <- withRenderPass device zero
    { attachments  = [albedoAttachmentDescription, normalAttachmentDescription, materialAttachmentDescription]
    , subpasses    = [subpass]
    , dependencies = [dependency]
    } Nothing mkAcquire
  debugName vulkanResources renderPass "DecalRenderPass"

  let samples = SAMPLE_COUNT_1_BIT
      renderPassInfo = Left renderPass
  pure RenderConfig {..}
  where
  albedoAttachmentDescription :: AttachmentDescription
  albedoAttachmentDescription = zero
    { format         = hdrFormat
    , samples        = SAMPLE_COUNT_1_BIT
    , loadOp         = ATTACHMENT_LOAD_OP_LOAD
    , storeOp        = ATTACHMENT_STORE_OP_STORE
    , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
    , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
    , initialLayout  = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
    , finalLayout    = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
    }
  normalAttachmentDescription :: AttachmentDescription
  normalAttachmentDescription = zero
    { format         = hdrFormat
    , samples        = SAMPLE_COUNT_1_BIT
    , loadOp         = ATTACHMENT_LOAD_OP_LOAD
    , storeOp        = ATTACHMENT_STORE_OP_STORE
    , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
    , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
    , initialLayout  = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
    , finalLayout    = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
    }
  materialAttachmentDescription = normalAttachmentDescription
  subpass :: SubpassDescription
  subpass = zero
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
    }
  dependency :: SubpassDependency
  dependency = zero
    { srcSubpass    = SUBPASS_EXTERNAL
    , dstSubpass    = 0
    , srcStageMask  = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    , srcAccessMask = ACCESS_COLOR_ATTACHMENT_WRITE_BIT
    , dstStageMask  = PIPELINE_STAGE_FRAGMENT_SHADER_BIT
    , dstAccessMask = ACCESS_SHADER_READ_BIT
    }

decalVertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
$header
$worldGlobalsDef
$decalUniformsDef
$instancedUniformDef

layout(location = 0) in vec3 inPosition;
layout(location = 2) out flat uint outUniformIdx;

void main()
{
  vec4 modelPos = vec4(inPosition,1.0);
  vec4 worldPosition = uniforms.modelMat * modelPos;
  gl_Position = globals.viewProjMat
              * worldPosition;
  outUniformIdx = uniformIdx;
}

|])

decalFragShader = $(compileShaderQ Nothing "frag" Nothing [qm|
$header
$worldGlobalsDef
$decalUniformsDef

layout (location = 2) in flat uint inUniformIdx;

layout (location = 0) out vec4 outColor;
layout (location = 1) out vec4 outNormal;

layout (set = 1, binding = 3) uniform usampler2D objIdSampler;
layout (set = 1, binding = 4) uniform sampler2D depthSampler;
layout (set = 2, binding = 0) uniform sampler2D decal;

layout (row_major, scalar, set = 1, binding = 0) uniform UniformBlock { Uniforms uniforms [128]; } uniformBlock;
Uniforms uniforms = uniformBlock.uniforms[inUniformIdx];

float linearizeDepth(float depth, float nearPlane, float farPlane)
{
    return nearPlane * farPlane / (farPlane - depth * (farPlane - nearPlane));
}

void main()
{
  vec2 screenUV = gl_FragCoord.xy / globals.gbufferSize;

  uint objId = texture(objIdSampler, screenUV).r;
  if (objId != uniforms.receiverId) {
    discard;
  }

  float depth = texture(depthSampler, screenUV).r;
  vec2 ndc = screenUV * 2.0 - 1.0;
  vec4 clipPos = vec4(ndc, depth, 1.0);
  vec4 decalPos = uniforms.invModelViewProjMat * clipPos;
  decalPos /= decalPos.w;
  if (abs(decalPos.x) > 0.5 || abs(decalPos.y) > 0.5 || abs(decalPos.z) > 0.5) {
    discard;
  }
  vec2 uv = decalPos.xy + 0.5;

  vec4 albedo = texture(decal, uv) * uniforms.color;

  outColor = albedo;
  outNormal = vec4(0,0,1,albedo.a);
}
|])
