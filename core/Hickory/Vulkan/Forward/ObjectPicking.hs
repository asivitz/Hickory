{-# LANGUAGE DataKinds, PatternSynonyms, QuasiQuotes  #-}
{-# LANGUAGE PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE DataKinds, DeriveGeneric, DerivingStrategies, DeriveAnyClass, OverloadedLists, OverloadedLabels #-}

module Hickory.Vulkan.Forward.ObjectPicking where

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
  , Filter (..), SamplerAddressMode (..), CullModeFlagBits (..), ImageUsageFlagBits (..)
  )
import Vulkan.Zero
import Acquire.Acquire (Acquire)
import qualified Data.Vector as V
import Data.Generics.Labels ()
import Hickory.Vulkan.Textures (withIntermediateImage, withImageSampler)
import Data.Bits (zeroBits)
import Hickory.Vulkan.Material (pipelineDefaults, PipelineOptions(..))
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag)
import Hickory.Vulkan.Types
import Hickory.Vulkan.RenderPass (createFramebuffer)
import Hickory.Vulkan.Monad (BufferedUniformMaterial, withBufferedUniformMaterial)
import GHC.Generics (Generic)
import Foreign.Storable.Generic (GStorable)
import Linear (M44)
import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Shaderc (vert)
import Hickory.Vulkan.Mesh (Attribute(..))

-- For e.g. mouse picking objects in scene
withObjectIDRenderTarget :: VulkanResources -> Swapchain -> Acquire RenderTarget
withObjectIDRenderTarget vulkanResources@VulkanResources { deviceContext = deviceContext@DeviceContext{..} } Swapchain {..} = do
  renderPass <- withRenderPass device zero
    { attachments  = [depthDescription, objIDDescription]
    , subpasses    = [subpass]
    , dependencies = [dependency]
    } Nothing mkAcquire

  depthImageRaw  <- withDepthImage vulkanResources extent depthFormat samples zeroBits
  depthImageView <- with2DImageView deviceContext depthFormat IMAGE_ASPECT_DEPTH_BIT depthImageRaw

  objIDImageRaw  <- withIntermediateImage vulkanResources objIDFormat IMAGE_USAGE_COLOR_ATTACHMENT_BIT extent samples
  objIDImageView <- with2DImageView deviceContext objIDFormat IMAGE_ASPECT_COLOR_BIT objIDImageRaw
  let objIDImage = ViewableImage objIDImageRaw objIDImageView objIDFormat
  sampler <- withImageSampler vulkanResources FILTER_LINEAR SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE

  frameBuffer <- createFramebuffer device renderPass extent [depthImageView, objIDImageView]
  let frameBuffers = V.replicate 3 frameBuffer
      cullMode = CULL_MODE_FRONT_BIT
      descriptorSpec = ImageDescriptor [(objIDImage,sampler)]

  pure RenderTarget {..}
  where
  samples = SAMPLE_COUNT_1_BIT
  objIDFormat     = FORMAT_R16_UINT
  depthFormat = FORMAT_D16_UNORM
  subpass :: SubpassDescription
  subpass = zero
    { pipelineBindPoint = PIPELINE_BIND_POINT_GRAPHICS
    , colorAttachments =
      [ zero
        { attachment = 1
        , layout     = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        }
      ]
    , depthStencilAttachment = Just $ zero
      { attachment = 0
      , layout     = IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
      }
    }
  dependency :: SubpassDependency
  dependency = zero
    { srcSubpass    = SUBPASS_EXTERNAL
    , dstSubpass    = 0
    , srcStageMask  = PIPELINE_STAGE_FRAGMENT_SHADER_BIT
    , srcAccessMask = zero
    , dstStageMask  = PIPELINE_STAGE_FRAGMENT_SHADER_BIT
    , dstAccessMask = ACCESS_SHADER_READ_BIT
    }
  depthDescription :: AttachmentDescription
  depthDescription = zero
    { format         = depthFormat
    , samples        = samples
    , loadOp         = ATTACHMENT_LOAD_OP_CLEAR
    , storeOp        = ATTACHMENT_STORE_OP_DONT_CARE
    , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
    , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
    , initialLayout  = IMAGE_LAYOUT_UNDEFINED
    , finalLayout    = IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL
    }
  objIDDescription :: AttachmentDescription
  objIDDescription = zero
    { format         = objIDFormat
    , samples        = samples
    , loadOp         = ATTACHMENT_LOAD_OP_CLEAR
    , storeOp        = ATTACHMENT_STORE_OP_STORE
    , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
    , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
    , initialLayout  = IMAGE_LAYOUT_UNDEFINED
    , finalLayout    = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
    }

data ObjectIDConstants = ObjectIDConstants
  { modelViewProjMat :: M44 Float
  , objectID         :: Int
  } deriving Generic
    deriving anyclass GStorable

withObjectIDMaterial :: VulkanResources -> RenderTarget -> PointedDescriptorSet -> Acquire (BufferedUniformMaterial ObjectIDConstants)
withObjectIDMaterial vulkanResources renderTarget globalDS
  = withBufferedUniformMaterial vulkanResources renderTarget [Position] (pipelineDefaults { blendEnable = False }) vertShader fragShader globalDS Nothing
  where
  vertShader :: ByteString
  vertShader = [vert|
  #version 450
  #extension GL_EXT_scalar_block_layout : require

  layout(location = 0) in vec3 inPosition;
  layout(location = 0) out uint objectID;

  struct Uniforms
  {
    mat4 modelMat;
    uint objectID;
  };

  layout (push_constant) uniform constants { uint uniformIdx; } PushConstants;
  layout (row_major, scalar, set = 1, binding = 0) uniform UniformBlock { Uniforms uniforms [128]; } uniformBlock;

  layout (row_major, scalar, set = 0, binding = 0) uniform GlobalUniform
    { mat4 viewMat;
      mat4 projMat;
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
      objectID = uniforms.objectID;
  }

|]

  fragShader :: ByteString
  fragShader = [frag|
  #version 450

  layout(location = 0) flat in uint objectID;
  layout(location = 0) out uint outColor;

  void main() {
    outColor = objectID;
  }
  |]
