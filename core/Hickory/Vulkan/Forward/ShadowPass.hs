{-# LANGUAGE DataKinds, PatternSynonyms, QuasiQuotes  #-}
{-# LANGUAGE PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE DataKinds, DeriveGeneric, DerivingStrategies, DeriveAnyClass, OverloadedLists, OverloadedLabels #-}

module Hickory.Vulkan.Forward.ShadowPass where

import Hickory.Vulkan.Vulkan (mkAcquire, ViewableImage(..), VulkanResources (..), DeviceContext (..), withDepthImage, with2DImageView)
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
import qualified Data.Vector as V
import Data.Generics.Labels ()
import Hickory.Vulkan.Textures (withShadowSampler)
import Data.Bits ((.|.))
import Hickory.Vulkan.Material (pipelineDefaults)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag)
import Hickory.Vulkan.Types
import Hickory.Vulkan.RenderPass (createFramebuffer)
import Hickory.Vulkan.Monad (BufferedUniformMaterial, withBufferedUniformMaterial)
import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Shaderc (vert)
import Hickory.Vulkan.Mesh (Attribute(..))
import Hickory.Vulkan.Forward.Types (AnimatedConstants, StaticConstants)
import Hickory.Types (Size(..))

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

  -- Shadowmap depth texture
  shadowmapImageRaw  <- withDepthImage vulkanResources shadowDim depthFormat SAMPLE_COUNT_1_BIT (IMAGE_USAGE_SAMPLED_BIT .|. IMAGE_USAGE_INPUT_ATTACHMENT_BIT)
  shadowmapImageView <- with2DImageView deviceContext depthFormat IMAGE_ASPECT_DEPTH_BIT shadowmapImageRaw
  let image = ViewableImage shadowmapImageRaw shadowmapImageView depthFormat
  sampler <- withShadowSampler vulkanResources

  shadowFrameBuffer <- createFramebuffer device renderPass shadowDim [shadowmapImageView]
  let frameBuffers = V.replicate 3 shadowFrameBuffer
      descriptorSpec = DepthImageDescriptor image sampler
      extent = shadowDim
      cullMode = CULL_MODE_FRONT_BIT
      samples = SAMPLE_COUNT_1_BIT

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

withStaticShadowMaterial :: VulkanResources -> RenderTarget -> PointedDescriptorSet -> Acquire (BufferedUniformMaterial StaticConstants)
withStaticShadowMaterial vulkanResources renderTarget globalDS
  = withBufferedUniformMaterial vulkanResources renderTarget [Position] pipelineDefaults staticVertShader fragShader globalDS Nothing

withAnimatedShadowMaterial :: VulkanResources -> RenderTarget -> PointedDescriptorSet -> Acquire (BufferedUniformMaterial AnimatedConstants)
withAnimatedShadowMaterial vulkanResources renderTarget globalDS
  = withBufferedUniformMaterial vulkanResources renderTarget [Position, BoneIndex] pipelineDefaults animatedVertShader fragShader globalDS Nothing

staticVertShader :: ByteString
staticVertShader = [vert|
  #version 450
  #extension GL_EXT_scalar_block_layout : require

  layout(location = 0) in vec3 inPosition;

  layout(location = 0) out vec4 shadowCoord;

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

      vec4 worldPosition = uniforms.modelMat
                         * vec4(inPosition, 1.0);

      gl_Position = globals.projMat
                  * globals.viewMat
                  * worldPosition;
  }

|]

animatedVertShader :: ByteString
animatedVertShader = [vert|
  #version 450
  #extension GL_EXT_scalar_block_layout : require

  layout(location = 0) in vec3 inPosition;
  layout(location = 4) in float inBoneIndex;

  layout(location = 0) out vec4 shadowCoord;

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

  void main() {
      Uniforms uniforms = uniformBlock.uniforms[PushConstants.uniformIdx];

      vec4 worldPosition = uniforms.modelMat
                         * uniforms.boneMat[int(inBoneIndex)]
                         * vec4(inPosition, 1.0);

      gl_Position = globals.projMat
                  * globals.viewMat
                  * worldPosition;
  }

|]

-- For the shadowmap, we don't care about pixel color
fragShader :: ByteString
fragShader = [frag|
#version 450

layout(location = 0) out vec4 outColor;

void main() {
  outColor = vec4(1.0,1.0,1.0,1.0);
}
|]
