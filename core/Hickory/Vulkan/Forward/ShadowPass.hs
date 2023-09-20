{-# LANGUAGE DataKinds, PatternSynonyms, QuasiQuotes, TemplateHaskell  #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric, DerivingStrategies, DeriveAnyClass, OverloadedLists, OverloadedLabels #-}

module Hickory.Vulkan.Forward.ShadowPass where

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
  , CullModeFlagBits (..), ImageUsageFlagBits (..), Extent2D (..)
  )
import Vulkan.Zero
import Acquire.Acquire (Acquire)
import Data.Generics.Labels ()
import Hickory.Vulkan.Textures (withShadowSampler)
import Data.Bits ((.|.))
import Hickory.Vulkan.Material (pipelineDefaults, depthClampEnable)
import Hickory.Vulkan.Types
import Hickory.Vulkan.RenderPass (createFramebuffer)
import Hickory.Vulkan.Monad (BufferedUniformMaterial, withBufferedUniformMaterial)
import Data.ByteString (ByteString)
import Hickory.Vulkan.Forward.Types (AnimatedConstants, StaticConstants)
import Hickory.Types (Size(..))
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (compileShaderQ)
import Data.String.QM (qm)
import Hickory.Vulkan.Forward.ShaderDefinitions
import Hickory.Vulkan.Framing (FramedResource, frameResource)

depthFormat :: Format
depthFormat = FORMAT_D32_SFLOAT

shadowDim :: Extent2D
shadowDim = Extent2D 2048 2048

shadowMapSize :: Size Int
shadowMapSize = Size 2048 2048

withShadowRenderTarget :: VulkanResources -> Acquire RenderTarget
withShadowRenderTarget vulkanResources@VulkanResources { deviceContext = deviceContext@DeviceContext{..} } = do
  renderPass <- withRenderPass device zero
    { attachments  = [shadowmapAttachmentDescription]
    , subpasses    = [shadowSubpass]
    , dependencies = [shadowDependency]
    } Nothing mkAcquire

  let extent = shadowDim
      cullModeOverride = Just CULL_MODE_FRONT_BIT
      samples = SAMPLE_COUNT_1_BIT
  sampler <- withShadowSampler vulkanResources

  frameBuffers <- frameResource do
    -- Shadowmap depth texture
    shadowmapImageRaw  <- withDepthImage vulkanResources shadowDim depthFormat SAMPLE_COUNT_1_BIT (IMAGE_USAGE_SAMPLED_BIT .|. IMAGE_USAGE_INPUT_ATTACHMENT_BIT)
    shadowmapImageView <- with2DImageView deviceContext depthFormat IMAGE_ASPECT_DEPTH_BIT shadowmapImageRaw
    let image = ViewableImage shadowmapImageRaw shadowmapImageView depthFormat
    let descriptorSpecs = [DepthImageDescriptor image sampler]
    (,descriptorSpecs) <$> createFramebuffer device renderPass shadowDim [shadowmapImageView]

  pure RenderTarget {..}
  where
  shadowSubpass :: SubpassDescription
  shadowSubpass = zero
    { pipelineBindPoint = PIPELINE_BIND_POINT_GRAPHICS
    , depthStencilAttachment = Just $ zero
      { attachment = 0
      , layout     = IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
      }
    }
  shadowDependency :: SubpassDependency
  shadowDependency = zero
    { srcSubpass    = SUBPASS_EXTERNAL
    , dstSubpass    = 0
    , srcStageMask  = PIPELINE_STAGE_FRAGMENT_SHADER_BIT
    , srcAccessMask = zero
    , dstStageMask  = PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
    , dstAccessMask = ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
    }
  shadowmapAttachmentDescription :: AttachmentDescription
  shadowmapAttachmentDescription = zero
    { format         = depthFormat
    , samples        = SAMPLE_COUNT_1_BIT
    , loadOp         = ATTACHMENT_LOAD_OP_CLEAR
    , storeOp        = ATTACHMENT_STORE_OP_STORE
    , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
    , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
    , initialLayout  = IMAGE_LAYOUT_UNDEFINED
    , finalLayout    = IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL
    }

withStaticShadowMaterial :: VulkanResources -> RenderTarget -> FramedResource PointedDescriptorSet -> Acquire (BufferedUniformMaterial StaticConstants)
withStaticShadowMaterial vulkanResources renderTarget globalDS
  = withBufferedUniformMaterial vulkanResources renderTarget [Position] pipelineDefaults { depthClampEnable = True } staticVertShader whiteFragShader globalDS Nothing

withAnimatedShadowMaterial :: VulkanResources -> RenderTarget -> FramedResource PointedDescriptorSet -> Acquire (BufferedUniformMaterial AnimatedConstants)
withAnimatedShadowMaterial vulkanResources renderTarget globalDS
  = withBufferedUniformMaterial vulkanResources renderTarget [Position, JointIndices, JointWeights] pipelineDefaults { depthClampEnable = True } animatedVertShader whiteFragShader globalDS Nothing

staticVertShader :: ByteString
staticVertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
$header
$shadowPassGlobalsDef
$staticUniformsDef

layout(location = 0) in vec3 inPosition;

layout(location = 0) out vec4 shadowCoord;

void main() {
  vec4 worldPosition = uniforms.modelMat
                      * vec4(inPosition, 1.0);

  gl_Position = globals.projMat
              * globals.viewMat
              * worldPosition;
}

|])

animatedVertShader :: ByteString
animatedVertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
$header
$shadowPassGlobalsDef
$animatedUniformsDef

layout(location = 0) in vec3 inPosition;
layout(location = 6) in vec4 inJointIndices;
layout(location = 7) in vec4 inJointWeights;

layout(location = 0) out vec4 shadowCoord;

void main() {
  mat4 skinMat
    = inJointWeights.x * uniforms.boneMat[int(inJointIndices.x)]
    + inJointWeights.y * uniforms.boneMat[int(inJointIndices.y)]
    + inJointWeights.z * uniforms.boneMat[int(inJointIndices.z)]
    + inJointWeights.w * uniforms.boneMat[int(inJointIndices.w)];

  vec4 worldPosition = uniforms.modelMat
                      * skinMat
                      * vec4(inPosition, 1.0);

  gl_Position = globals.projMat
              * globals.viewMat
              * worldPosition;
}

|])

-- For the shadowmap, we don't care about pixel color
whiteFragShader :: ByteString
whiteFragShader = $(compileShaderQ Nothing "frag" Nothing [qm|
$header

layout(location = 0) out vec4 outColor;

void main() {
  outColor = vec4(1.0,1.0,1.0,1.0);
}
|])
