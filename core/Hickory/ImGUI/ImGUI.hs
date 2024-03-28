{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Hickory.ImGUI.ImGUI where

import Hickory.Vulkan.Vulkan
import Vulkan
  ( Instance
  , RenderPassBeginInfo(..)
  , pattern NULL_HANDLE, Result (..), SampleCountFlagBits (..), DescriptorType (..)
  , DescriptorPoolCreateInfo(..), withDescriptorPool, DescriptorPoolSize (..), AttachmentDescription, SubpassDescription, SubpassDependency
  , AttachmentDescription(..), SubpassDescription(..), SubpassDependency(..), AttachmentReference(..)
  , RenderPassCreateInfo(..), AttachmentLoadOp (..), AttachmentStoreOp (..), ImageLayout (..), PipelineBindPoint (..), Format, pattern SUBPASS_EXTERNAL, PipelineStageFlagBits (..), AccessFlagBits (..), withRenderPass, SurfaceFormatKHR(SurfaceFormatKHR), RenderPass, Rect2D (..), ClearValue (..), ClearColorValue (..), cmdUseRenderPass, SubpassContents (SUBPASS_CONTENTS_INLINE), Framebuffer
  )
import Acquire.Acquire (Acquire)
import Control.Monad.IO.Class (liftIO)
import DearImGui (createContext, destroyContext, render, getDrawData, newFrame)
import DearImGui.Vulkan (vulkanInit, vulkanShutdown, vulkanCreateFontsTexture, vulkanDestroyFontUploadObjects, vulkanNewFrame, vulkanRenderDrawData)
import qualified DearImGui.Vulkan as ImGui.Vulkan
import Control.Exception (throw)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Hickory.Vulkan.Mesh (withSingleTimeCommands)
import Control.Monad (void)
import Data.Traversable (for)
import Hickory.Vulkan.RenderPass (createFramebuffer)
import qualified Data.Vector as V
import Hickory.Vulkan.Types (FrameContext(..), VulkanResources (..), Swapchain (..), DeviceContext (..), ViewableImage (..))

data ImGuiResources = ImGuiResources
  { renderPass   :: RenderPass
  , frameBuffers :: V.Vector Framebuffer
  }

renderDearImGui :: ImGuiResources -> FrameContext -> IO () -> IO () -> IO ()
renderDearImGui ImGuiResources {..} FrameContext {..} newPlatformFrame f = do
  vulkanNewFrame
  newPlatformFrame
  newFrame
  f -- Renders imgui windows

  let
    renderPassBeginInfo = zero
        { renderPass  = renderPass
        , framebuffer = frameBuffers V.! fromIntegral swapchainImageIndex
        , renderArea  = Rect2D { offset = zero , extent = extent }
        , clearValues = [Color (Float32 0 0 0 0)]
        }
  cmdUseRenderPass commandBuffer renderPassBeginInfo SUBPASS_CONTENTS_INLINE do
    render
    drawData <- getDrawData
    vulkanRenderDrawData drawData commandBuffer Nothing

initDearImGui :: IO () -> IO () -> VulkanResources -> Swapchain -> Acquire ImGuiResources
initDearImGui platformInit platformShutdown vulkanResources@VulkanResources {..} Swapchain {..} = do
  _imguiContext <- mkAcquire (liftIO createContext) (liftIO . destroyContext)
  let
    DeviceContext {..} = deviceContext

  descriptorPool <- withDescriptorPool device zero
    { maxSets   = 1000
    , poolSizes =
      [ DescriptorPoolSize Vulkan.DESCRIPTOR_TYPE_SAMPLER 1000
      , DescriptorPoolSize Vulkan.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER 1000
      , DescriptorPoolSize Vulkan.DESCRIPTOR_TYPE_SAMPLED_IMAGE 1000
      , DescriptorPoolSize Vulkan.DESCRIPTOR_TYPE_STORAGE_IMAGE 1000
      , DescriptorPoolSize Vulkan.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER 1000
      , DescriptorPoolSize Vulkan.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER 1000
      , DescriptorPoolSize Vulkan.DESCRIPTOR_TYPE_UNIFORM_BUFFER 1000
      , DescriptorPoolSize Vulkan.DESCRIPTOR_TYPE_STORAGE_BUFFER 1000
      , DescriptorPoolSize Vulkan.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC 1000
      , DescriptorPoolSize Vulkan.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC 1000
      , DescriptorPoolSize Vulkan.DESCRIPTOR_TYPE_INPUT_ATTACHMENT 1000
      ]
    }
    Nothing
    mkAcquire

  let SurfaceFormatKHR format _ = imageFormat
  renderPass <- withRenderPass device zero
    { attachments  = [colorAttachmentDescription format]
    , subpasses    = [subpass]
    , dependencies = [subpassDependency]
    } Nothing mkAcquire

  _ <- mkAcquire platformInit (const platformShutdown)

  let
    initInfo :: ImGui.Vulkan.InitInfo
    initInfo = ImGui.Vulkan.InitInfo
      { instance' = inst
      , physicalDevice
      , device
      , queueFamily = graphicsFamilyIdx
      , queue = graphicsQueue
      , pipelineCache  = Vulkan.NULL_HANDLE
      , descriptorPool = descriptorPool
      , subpass        = 0
      , minImageCount  = 2
      , imageCount     = 2
      , msaaSamples    = SAMPLE_COUNT_1_BIT
      , mbAllocator    = Nothing
      , checkResult    = \case { SUCCESS -> pure (); e -> throw $ VulkanException e }
      }

  _ <- mkAcquire (liftIO $ vulkanInit initInfo renderPass) (liftIO . vulkanShutdown)

  let acqFonts = withSingleTimeCommands vulkanResources $ void . vulkanCreateFontsTexture
  _ <- mkAcquire acqFonts (const vulkanDestroyFontUploadObjects)

  frameBuffers <- for images \(ViewableImage _img imgView _format) ->
    createFramebuffer device renderPass extent [imgView]

  pure ImGuiResources {..}

  where
  colorAttachmentDescription :: Format -> AttachmentDescription
  colorAttachmentDescription format = zero
    { format         = format
    , samples        = SAMPLE_COUNT_1_BIT
    , loadOp         = ATTACHMENT_LOAD_OP_LOAD
    , storeOp        = ATTACHMENT_STORE_OP_STORE
    , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
    , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
    , initialLayout  = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
    , finalLayout    = IMAGE_LAYOUT_PRESENT_SRC_KHR
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
  subpassDependency :: SubpassDependency
  subpassDependency = zero
    { srcSubpass    = SUBPASS_EXTERNAL
    , dstSubpass    = 0
    , srcStageMask  = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    , srcAccessMask = zero
    , dstStageMask  = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    , dstAccessMask = ACCESS_COLOR_ATTACHMENT_WRITE_BIT
    }
