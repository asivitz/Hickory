{-# LANGUAGE PatternSynonyms, DuplicateRecordFields, OverloadedRecordDot #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds, OverloadedLists #-}

module Hickory.Vulkan.RenderPass where

-- With VK_KHR_dynamic_rendering, we might not actually need to create
-- renderpasses and framebuffers
-- (Only needed really if we need to take advantage of subpasses for
-- performance benefits.)

import Hickory.Vulkan.Vulkan (mkAcquire)
import qualified Vulkan
import Vulkan
  ( Device
  , Extent2D (..)

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
  , FramebufferCreateInfo(..)
  , ImageView
  , Framebuffer
  , withFramebuffer

  , ClearValue(..)
  , Rect2D (..), RenderPassBeginInfo(..)
  , cmdUseRenderPass
  , pattern SUBPASS_CONTENTS_INLINE
  , RenderPass, CullModeFlagBits (..)
  )
import Vulkan.Zero
import Acquire (Acquire)
import qualified Data.Vector as V
import Data.Generics.Labels ()
import Data.Traversable (for)
import Control.Monad.IO.Class (MonadIO)
import Hickory.Vulkan.Types
import Hickory.Vulkan.Framing (resourceForFrame, FramedResource)
import Data.Word (Word32)

renderConfigRenderPass :: RenderConfig -> RenderPass
renderConfigRenderPass RenderConfig {..} = case renderPassInfo of
  Left rp -> rp
  Right _ -> error "Trying to get a render pass from a dynamic rendering render config"

withSwapchainFramebuffers :: VulkanResources -> Swapchain -> RenderConfig -> Acquire (FramedResource Framebuffer)
withSwapchainFramebuffers VulkanResources { deviceContext = DeviceContext{..} } sc RenderConfig {..} = do
  let renderPass = case renderPassInfo of
        Left rp -> rp
        Right _ -> error "Trying to use a dynamic render render config to make a framebuffer"
  for sc.images \(ViewableImage _img imgView _format) ->
    createFramebuffer device renderPass extent [imgView]

withSwapchainRenderConfig :: VulkanResources -> Swapchain -> Acquire RenderConfig
withSwapchainRenderConfig VulkanResources { deviceContext = DeviceContext{..} } Swapchain {..} = do
  renderPass <- withRenderPass device zero
    { attachments  = [outColorAttachmentDescription]
    , subpasses    = [postOverlaySubpass]
    , dependencies = [postOverlayDependency]
    } Nothing mkAcquire

  let cullModeOverride = Nothing
      samples = SAMPLE_COUNT_1_BIT
      renderPassInfo = Left renderPass

  pure RenderConfig {..}
  where
  outColorAttachmentDescription :: AttachmentDescription
  outColorAttachmentDescription = zero
    { format         = imageFormat.format
    , samples        = SAMPLE_COUNT_1_BIT
    , loadOp         = ATTACHMENT_LOAD_OP_DONT_CARE
    , storeOp        = ATTACHMENT_STORE_OP_STORE
    , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
    , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
    , initialLayout  = IMAGE_LAYOUT_UNDEFINED
    , finalLayout    = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL -- Leave as attachment for DearImgui, which will present it
    }
  postOverlaySubpass :: SubpassDescription
  postOverlaySubpass = zero
    { pipelineBindPoint = PIPELINE_BIND_POINT_GRAPHICS
    , colorAttachments =
      [ zero
        { attachment = 0
        , layout     = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        }
      ]
    , depthStencilAttachment = Nothing
    }
  postOverlayDependency :: SubpassDependency
  postOverlayDependency = zero
    { srcSubpass    = SUBPASS_EXTERNAL
    , dstSubpass    = 0
    , srcStageMask  = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    , srcAccessMask = ACCESS_COLOR_ATTACHMENT_WRITE_BIT
    , dstStageMask  = PIPELINE_STAGE_FRAGMENT_SHADER_BIT
    , dstAccessMask = ACCESS_SHADER_READ_BIT
    }

createFramebuffer :: Device -> RenderPass -> Extent2D -> V.Vector ImageView -> Acquire Framebuffer
createFramebuffer dev renderPass swapchainExtent imageViews =
  let framebufferCreateInfo :: FramebufferCreateInfo '[]
      framebufferCreateInfo = zero
        { renderPass  = renderPass
        , attachments = imageViews
        , width       = swapchainExtent.width
        , height      = swapchainExtent.height
        , layers      = 1
        }
  in withFramebuffer dev framebufferCreateInfo Nothing mkAcquire

useRenderConfig :: (MonadIO io) => RenderConfig -> Vulkan.CommandBuffer -> V.Vector ClearValue -> Word32 -> FramedResource Framebuffer -> io r -> io r
useRenderConfig RenderConfig {..} commandBuffer clearValues swapchainImageIndex frameBuffers f = do
  let
      renderPassBeginInfo = zero
        { renderPass  = case renderPassInfo of
          Left renderPass -> renderPass
          Right _ -> error "Trying to use dynamic rendering render config in a render pass"
        , framebuffer = resourceForFrame swapchainImageIndex frameBuffers
        , renderArea  = Rect2D { offset = zero , extent = extent }
        , clearValues = clearValues
        }

  cmdUseRenderPass commandBuffer renderPassBeginInfo SUBPASS_CONTENTS_INLINE f
