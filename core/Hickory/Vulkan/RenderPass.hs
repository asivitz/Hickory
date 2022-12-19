{-# LANGUAGE PatternSynonyms, DuplicateRecordFields #-}
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
  , SurfaceFormatKHR

  , ClearValue(..)
  , Rect2D (..), RenderPassBeginInfo(..)
  , cmdUseRenderPass
  , pattern SUBPASS_CONTENTS_INLINE
  , RenderPass, CullModeFlagBits (..)
  )
import Vulkan.Zero
import Acquire.Acquire (Acquire)
import qualified Data.Vector as V
import Data.Generics.Labels ()
import Data.Traversable (for)
import Control.Monad.IO.Class (MonadIO)
import Hickory.Vulkan.Types
import Data.Maybe

withSwapchainRenderTarget :: VulkanResources -> Swapchain -> Acquire RenderTarget
withSwapchainRenderTarget VulkanResources { deviceContext = DeviceContext{..} } Swapchain {..} = do
  renderPass <- withRenderPass device zero
    { attachments  = [outColorAttachmentDescription]
    , subpasses    = [postOverlaySubpass]
    , dependencies = [postOverlayDependency]
    } Nothing mkAcquire

  frameBuffers <- for images \(ViewableImage _img imgView _format) ->
    createFramebuffer device renderPass extent [imgView]

  let descriptorSpecs = []
      cullMode = CULL_MODE_BACK_BIT
      samples = SAMPLE_COUNT_1_BIT

  pure RenderTarget {..}
  where
  outColorAttachmentDescription :: AttachmentDescription
  outColorAttachmentDescription = zero
    { format         = Vulkan.format (imageFormat :: SurfaceFormatKHR)
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
        , width       = width (swapchainExtent :: Extent2D)
        , height      = height (swapchainExtent :: Extent2D)
        , layers      = 1
        }
  in withFramebuffer dev framebufferCreateInfo Nothing mkAcquire

useRenderTarget :: (MonadIO io, Integral a) => RenderTarget -> Vulkan.CommandBuffer -> V.Vector ClearValue -> a -> io r -> io r
useRenderTarget RenderTarget {..} commandBuffer clearValues swapchainImageIndex f = do
  let framebuffer = fromMaybe (error "Error accessing framebuffer") $ frameBuffers V.!? fromIntegral swapchainImageIndex
      renderPassBeginInfo = zero
        { renderPass  = renderPass
        , framebuffer = framebuffer
        , renderArea  = Rect2D { offset = zero , extent = extent }
        , clearValues = clearValues
        }

  cmdUseRenderPass commandBuffer renderPassBeginInfo SUBPASS_CONTENTS_INLINE f
