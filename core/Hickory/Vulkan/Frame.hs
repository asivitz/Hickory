{-# LANGUAGE BlockArguments, ScopedTypeVariables, RecordWildCards, PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE DataKinds, OverloadedLists, OverloadedLabels #-}

module Hickory.Vulkan.Frame where

import Control.Lens (view, (<&>))
import Control.Monad
import Vulkan
  ( AttachmentLoadOp (..)
  , AttachmentStoreOp (..)
  , PipelineStageFlagBits (..)
  , withCommandPool
  , CommandPoolCreateInfo(..), CommandPoolCreateFlagBits (..), CommandBufferAllocateInfo(..), CommandBufferLevel (..), withCommandBuffers
  , withSemaphore
  , withFence
  , FenceCreateInfo(..)
  , FenceCreateFlagBits (..)
  , CommandBuffer(..)



  , Rect2D (..)
  , SubmitInfo(..)
  , PresentInfoKHR(..), resetCommandBuffer, acquireNextImageKHR, useCommandBuffer, queueSubmit, queuePresentKHR, ClearValue (..), ClearColorValue (..), waitForFences, resetFences, Result (..)
  , ClearDepthStencilValue (..)
  , RenderingInfo(..)
  , RenderingAttachmentInfo(..)
  , cmdBeginRenderingKHR
  , cmdEndRenderingKHR, pattern IMAGE_LAYOUT_ATTACHMENT_OPTIMAL_KHR, pattern IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL_KHR, Extent2D, SampleCountFlagBits (..)
  )
import Vulkan.Zero
import qualified Data.Vector as V
import Vulkan.CStruct.Extends (SomeStruct(..))
import Linear (V4(..))
import Hickory.Vulkan.Vulkan (mkAcquire)
import Data.Generics.Labels ()
import Acquire (Acquire)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (SomeException, handle, throw)
import Hickory.Vulkan.Types (DeviceContext(..), Frame (..), ViewableImage, VulkanResources (..), Swapchain (..), FrameContext (..))

{- FRAME -}

withFrame :: DeviceContext -> Acquire Frame
withFrame DeviceContext {..} = do
  commandPool <-
    let commandPoolCreateInfo :: CommandPoolCreateInfo
        commandPoolCreateInfo = zero { queueFamilyIndex = graphicsFamilyIdx, flags = COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT }
    in withCommandPool device commandPoolCreateInfo Nothing mkAcquire

  commandBuffer <- V.head <$>
    let commandBufferAllocateInfo :: CommandBufferAllocateInfo
        commandBufferAllocateInfo = zero
          { commandPool        = commandPool
          , level              = COMMAND_BUFFER_LEVEL_PRIMARY
          , commandBufferCount = 1
          }
    in withCommandBuffers device commandBufferAllocateInfo mkAcquire

  imageAvailableSemaphore <- withSemaphore device zero Nothing mkAcquire
  renderFinishedSemaphore <- withSemaphore device zero Nothing mkAcquire
  inFlightFence           <- withFence device zero { flags = FENCE_CREATE_SIGNALED_BIT } Nothing mkAcquire

  pure Frame {..}

useDynamicRenderPass :: MonadIO m => CommandBuffer -> Extent2D -> V4 Float -> ViewableImage -> Maybe ViewableImage -> m () -> m ()
useDynamicRenderPass commandBuffer swapchainExtent (V4 r g b a) image depthImage f = do
  cmdBeginRenderingKHR commandBuffer zero
    { renderArea = Rect2D { offset = zero , extent = swapchainExtent }
    , layerCount = 1
    , colorAttachments = [ zero
      { imageView   = view #imageView image
      , imageLayout = IMAGE_LAYOUT_ATTACHMENT_OPTIMAL_KHR
      , loadOp      = ATTACHMENT_LOAD_OP_CLEAR
      , storeOp     = ATTACHMENT_STORE_OP_STORE
      , clearValue  = Color (Float32 r g b a)
      }
      ]
    , depthAttachment = depthImage <&> \di -> zero
      { imageView   = view #imageView di
      , imageLayout = IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL_KHR
      , loadOp      = ATTACHMENT_LOAD_OP_CLEAR
      , storeOp     = ATTACHMENT_STORE_OP_STORE
      , clearValue  = DepthStencil (ClearDepthStencilValue 1 0)
      }
    }
  f
  cmdEndRenderingKHR commandBuffer

-- |Draw a frame
-- Handles concerns around synchronizing double buffers
-- Returns False if swapchain is stale and needs to be refreshed
drawFrame :: MonadIO m => Int -> Frame -> VulkanResources -> Swapchain -> (FrameContext -> IO ()) -> m Bool
drawFrame frameNumber Frame {..} VulkanResources {..} swapchain f = do
  let Swapchain {..} = swapchain
      DeviceContext {..} = deviceContext

  _ <- waitForFences device [ inFlightFence ] True maxBound

  (res, imageIndex) <- acquireNextImageKHR device swapchainHandle maxBound imageAvailableSemaphore zero
  case res of
    res' | res' == ERROR_OUT_OF_DATE_KHR || res' == SUBOPTIMAL_KHR -> pure False
    _ -> do
      let image = images V.! fromIntegral imageIndex
      resetFences device [ inFlightFence ]

      resetCommandBuffer commandBuffer zero

      useCommandBuffer commandBuffer zero do
        let samples = case maxSampleCount of
              SAMPLE_COUNT_64_BIT -> 64
              SAMPLE_COUNT_32_BIT -> 32
              SAMPLE_COUNT_16_BIT -> 16
              SAMPLE_COUNT_8_BIT -> 8
              SAMPLE_COUNT_4_BIT -> 4
              SAMPLE_COUNT_2_BIT -> 2
              SAMPLE_COUNT_1_BIT -> 1
              _ -> 1

        liftIO $ handle (\(e :: SomeException) -> putStrLn ("ERROR: " ++ show e) >> throw e) do
          f (FrameContext extent image commandBuffer frameNumber imageIndex samples)

      let submitInfo = zero
            { waitSemaphores   = [imageAvailableSemaphore]
            , waitDstStageMask = [PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
            , commandBuffers   = [commandBufferHandle commandBuffer]
            , signalSemaphores = [renderFinishedSemaphore]
            }
      queueSubmit graphicsQueue [SomeStruct submitInfo] inFlightFence
      void $ queuePresentKHR presentQueue $ zero
        { waitSemaphores = [renderFinishedSemaphore]
        , swapchains     = [swapchainHandle]
        , imageIndices   = [imageIndex]
        }
      pure True
