{-# LANGUAGE DataKinds, PatternSynonyms, QuasiQuotes, TemplateHaskell  #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric, DerivingStrategies, DeriveAnyClass, OverloadedLists, OverloadedLabels #-}

module Hickory.Vulkan.Renderer.ShadowPass where

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
  , ImageUsageFlagBits (..), Extent2D (..), Framebuffer, ImageViewType (..)
  )
import Vulkan.Zero
import Acquire (Acquire)
import Data.Generics.Labels ()
import Hickory.Vulkan.Textures (withShadowSampler)
import Data.Bits ((.|.))
import Hickory.Vulkan.Types
import Hickory.Vulkan.RenderPass (createFramebuffer, renderConfigRenderPass)
import Data.ByteString (ByteString)
import Hickory.Types (Size(..))
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (compileShaderQ)
import Data.String.QM (qm)
import Hickory.Vulkan.Renderer.ShaderDefinitions
import qualified Data.Vector.Sized as VS
import Data.Finite (getFinite)

depthFormat :: Format
depthFormat = FORMAT_D32_SFLOAT

shadowDim :: Extent2D
shadowDim = Extent2D 2048 2048

shadowMapSize :: Size Int
shadowMapSize = Size 2048 2048

withShadowMap :: VulkanResources -> RenderConfig -> Acquire (VS.Vector MaxShadowCascadesNat(Framebuffer, DescriptorSpec), DescriptorSpec)
withShadowMap vulkanResources@VulkanResources { deviceContext = deviceContext@DeviceContext{..} } rc = do
  sampler <- withShadowSampler vulkanResources
  -- Shadowmap depth texture
  shadowmapImageRaw  <- withDepthImage vulkanResources shadowDim depthFormat SAMPLE_COUNT_1_BIT (IMAGE_USAGE_SAMPLED_BIT .|. IMAGE_USAGE_INPUT_ATTACHMENT_BIT) maxShadowCascades
  shadowmapImageView <- with2DImageView deviceContext depthFormat IMAGE_ASPECT_DEPTH_BIT shadowmapImageRaw IMAGE_VIEW_TYPE_2D_ARRAY 0 maxShadowCascades
  let image = ViewableImage shadowmapImageRaw shadowmapImageView depthFormat
  let shadowmapDescriptorSpec = DepthImageDescriptor image sampler

  cascades <- VS.generateM \(fromIntegral . getFinite -> i) -> do
    cascadeImageView <- with2DImageView deviceContext depthFormat IMAGE_ASPECT_DEPTH_BIT shadowmapImageRaw IMAGE_VIEW_TYPE_2D i 1
    let cascadeImage = ViewableImage shadowmapImageRaw cascadeImageView depthFormat
    let descriptorSpec = DepthImageDescriptor cascadeImage sampler

    (,descriptorSpec) <$> createFramebuffer device (renderConfigRenderPass rc) shadowDim [cascadeImageView]
  pure (cascades, shadowmapDescriptorSpec)

withShadowRenderConfig :: VulkanResources -> Acquire RenderConfig
withShadowRenderConfig vulkanResources@VulkanResources { deviceContext = deviceContext@DeviceContext{..} } = do
  renderPass <- withRenderPass device zero
    { attachments  = [shadowmapAttachmentDescription]
    , subpasses    = [shadowSubpass]
    , dependencies = [shadowDependency]
    } Nothing mkAcquire

  let extent = shadowDim
      samples = SAMPLE_COUNT_1_BIT
      renderPassInfo = Left renderPass

  pure RenderConfig {..}
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

staticVertShader :: ByteString
staticVertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
$header
$shadowPassGlobalsDef
$shadowPushConstantsDef
$staticUniformsDef

layout(location = 0) in vec3 inPosition;

layout(location = 0) out vec4 shadowCoord;

void main() {
  vec4 worldPosition = uniforms.modelMat
                      * vec4(inPosition, 1.0);

  gl_Position = shadowGlobals.viewProjMat[PushConstants.cascadeIndex]
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
