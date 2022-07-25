{-# LANGUAGE BlockArguments, ScopedTypeVariables, RecordWildCards, PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE DataKinds, OverloadedLists #-}

module Hickory.Vulkan.Frame where

import Control.Monad
import Control.Monad.Managed
import Vulkan
  ( AttachmentLoadOp (..)
  , AttachmentStoreOp (..)
  , ImageLayout (..)
  , PipelineStageFlagBits (..)
  , withCommandPool
  , CommandPoolCreateInfo(..), CommandPoolCreateFlagBits (..), CommandBufferAllocateInfo(..), CommandBufferLevel (..), withCommandBuffers
  , withSemaphore
  , withFence
  , FenceCreateInfo(..)
  , FenceCreateFlagBits (..)
  , CommandBuffer(..)
  , Semaphore
  , Fence
  , CommandPool
  , Rect2D (..)
  , SubmitInfo(..)
  , PresentInfoKHR(..), resetCommandBuffer, acquireNextImageKHR, useCommandBuffer, queueSubmit, queuePresentKHR, ClearValue (..), ClearColorValue (..), waitForFences, resetFences, Result (..)
  , ClearDepthStencilValue (..)
  , RenderingInfo(..)
  , RenderingAttachmentInfo(..)
  , cmdBeginRenderingKHR
  , cmdEndRenderingKHR, pattern IMAGE_LAYOUT_ATTACHMENT_OPTIMAL_KHR, pattern IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL_KHR
  )
import Vulkan.Zero
import qualified Data.Vector as V

import Vulkan.CStruct.Extends (SomeStruct(..))
import Linear (V4(..))
import Hickory.Vulkan.Textures (transitionImageLayout)
import Hickory.Vulkan.Vulkan (DeviceContext (..), VulkanResources (..), Swapchain (..), allocate)

-- |Contains resources needed to render a frame. Need two of these for 'Double Buffering'.
data Frame = Frame
  { imageAvailableSemaphore :: Semaphore
  , renderFinishedSemaphore :: Semaphore
  , inFlightFence           :: Fence
  , commandPool             :: CommandPool
  , commandBuffer           :: CommandBuffer
  }

{- FRAME -}

withFrame :: DeviceContext -> Managed Frame
withFrame DeviceContext {..} = do
  commandPool <-
    let commandPoolCreateInfo :: CommandPoolCreateInfo
        commandPoolCreateInfo = zero { queueFamilyIndex = graphicsFamilyIdx, flags = COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT }
    in withCommandPool device commandPoolCreateInfo Nothing allocate

  commandBuffer <- V.head <$>
    let commandBufferAllocateInfo :: CommandBufferAllocateInfo
        commandBufferAllocateInfo = zero
          { commandPool        = commandPool
          , level              = COMMAND_BUFFER_LEVEL_PRIMARY
          , commandBufferCount = 1
          }
    in withCommandBuffers device commandBufferAllocateInfo allocate

  imageAvailableSemaphore <- withSemaphore device zero Nothing allocate
  renderFinishedSemaphore <- withSemaphore device zero Nothing allocate
  inFlightFence           <- withFence device zero { flags = FENCE_CREATE_SIGNALED_BIT } Nothing allocate

  pure Frame {..}

{- Drawing a frame -}
drawFrame :: MonadIO m => Frame -> VulkanResources -> Swapchain -> V4 Float -> (CommandBuffer -> IO ()) -> m Bool
drawFrame Frame {..} VulkanResources {..} swapchain (V4 r g b a) f = do
  let Swapchain {..} = swapchain
      DeviceContext {..} = deviceContext

  _ <- waitForFences device [ inFlightFence ] True maxBound

  (res, imageIndex) <- acquireNextImageKHR device swapchainHandle maxBound imageAvailableSemaphore zero
  case res of
    res' | res' == ERROR_OUT_OF_DATE_KHR || res' == SUBOPTIMAL_KHR -> pure False
    _ -> do
      resetFences device [ inFlightFence ]

      resetCommandBuffer commandBuffer zero

      useCommandBuffer commandBuffer zero do

        transitionImageLayout (images V.! fromIntegral imageIndex) IMAGE_LAYOUT_UNDEFINED IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL commandBuffer
        transitionImageLayout depthImage IMAGE_LAYOUT_UNDEFINED IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL commandBuffer

        cmdBeginRenderingKHR commandBuffer zero
          { renderArea = Rect2D { offset = zero , extent = extent }
          , layerCount = 1
          , colorAttachments = [ zero
            { imageView   = imageViews V.! fromIntegral imageIndex
            , imageLayout = IMAGE_LAYOUT_ATTACHMENT_OPTIMAL_KHR
            , loadOp      = ATTACHMENT_LOAD_OP_CLEAR
            , storeOp     = ATTACHMENT_STORE_OP_STORE
            , clearValue  = Color (Float32 r g b a)
            }
            ]
          , depthAttachment = Just $ zero
            { imageView   = depthImageView
            , imageLayout = IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL_KHR
            , loadOp      = ATTACHMENT_LOAD_OP_CLEAR
            , storeOp     = ATTACHMENT_STORE_OP_STORE
            , clearValue  = DepthStencil (ClearDepthStencilValue 1 0)
            }
          }
        liftIO $ f commandBuffer
        cmdEndRenderingKHR commandBuffer

        transitionImageLayout (images V.! fromIntegral imageIndex) IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL IMAGE_LAYOUT_PRESENT_SRC_KHR commandBuffer

{-
        let renderPassBeginInfo = zero
              { renderPass  = renderpass
              , framebuffer = framebuffer
              , renderArea  = Rect2D { offset = zero , extent = extent }
              , clearValues = [ Color (Float32 r g b a), DepthStencil (ClearDepthStencilValue 1 0) ]
              }
        cmdUseRenderPass commandBuffer renderPassBeginInfo SUBPASS_CONTENTS_INLINE do
          liftIO $ f commandBuffer
          -}

      let submitInfo = zero
            { waitSemaphores   = [imageAvailableSemaphore]
            , waitDstStageMask = [PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
            , commandBuffers   = [commandBufferHandle  commandBuffer]
            , signalSemaphores = [renderFinishedSemaphore]
            }
      queueSubmit graphicsQueue [SomeStruct submitInfo] inFlightFence
      void $ queuePresentKHR presentQueue $ zero
        { waitSemaphores = [renderFinishedSemaphore]
        , swapchains     = [swapchainHandle]
        , imageIndices   = [imageIndex]
        }
      pure True
