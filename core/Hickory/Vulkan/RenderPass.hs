{-# LANGUAGE PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE DataKinds, OverloadedLists #-}

module Hickory.Vulkan.RenderPass where

-- With VK_KHR_dynamic_rendering, we might not actually need to create
-- renderpasses and framebuffers
-- (Only needed really if we need to take advantage of subpasses for
-- performance benefits.)

import Hickory.Vulkan.Vulkan (mkAcquire)
import Vulkan
  ( Device
  , Extent2D (..)
  , Format (..)
  , RenderPass
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
  )
import Vulkan.Zero
import Foreign (Bits ((.|.)))
import Acquire.Acquire (Acquire)
import Data.Maybe (catMaybes)
import qualified Data.Vector as V

withStandardRenderPass' :: Device -> Format -> Format -> Acquire RenderPass
withStandardRenderPass' dev swapchainImageFormat depthFormat =
  withRenderPass dev zero
    { attachments  = [colorAttachmentDescription, depthAttachmentDescription]
    , subpasses    = [subpass]
    , dependencies = [subpassDependency]
    } Nothing mkAcquire
  where
  colorAttachmentDescription :: AttachmentDescription
  colorAttachmentDescription = zero
    { format         = swapchainImageFormat
    , samples        = SAMPLE_COUNT_1_BIT
    , loadOp         = ATTACHMENT_LOAD_OP_CLEAR
    , storeOp        = ATTACHMENT_STORE_OP_STORE
    , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
    , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
    , initialLayout  = IMAGE_LAYOUT_UNDEFINED
    , finalLayout    = IMAGE_LAYOUT_PRESENT_SRC_KHR
    }
  depthAttachmentDescription :: AttachmentDescription
  depthAttachmentDescription = zero
    { format         = depthFormat
    , samples        = SAMPLE_COUNT_1_BIT
    , loadOp         = ATTACHMENT_LOAD_OP_CLEAR
    , storeOp        = ATTACHMENT_STORE_OP_DONT_CARE
    , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
    , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
    , initialLayout  = IMAGE_LAYOUT_UNDEFINED
    , finalLayout    = IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
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
    , depthStencilAttachment = Just $ zero
      { attachment = 1
      , layout     = IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
      }
    }
  subpassDependency :: SubpassDependency
  subpassDependency = zero
    { srcSubpass    = SUBPASS_EXTERNAL
    , dstSubpass    = 0
    , srcStageMask  = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT .|. PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
    , srcAccessMask = zero
    , dstStageMask  = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT .|. PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
    , dstAccessMask = ACCESS_COLOR_ATTACHMENT_WRITE_BIT .|. ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
    }

createFramebuffer :: Device -> RenderPass -> Extent2D -> ImageView -> Maybe ImageView -> Acquire Framebuffer
createFramebuffer dev renderPass swapchainExtent imageView depthImageView =
  let framebufferCreateInfo :: FramebufferCreateInfo '[]
      framebufferCreateInfo = zero
        { renderPass  = renderPass
        , attachments = V.fromList $ catMaybes [Just imageView, depthImageView]
        , width       = width (swapchainExtent :: Extent2D)
        , height      = height (swapchainExtent :: Extent2D)
        , layers      = 1
        }
  in withFramebuffer dev framebufferCreateInfo Nothing mkAcquire
