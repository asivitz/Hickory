{-# LANGUAGE BlockArguments, ScopedTypeVariables, RecordWildCards, PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE DataKinds, OverloadedLists, OverloadedLabels #-}

module Hickory.Vulkan.Frame where

import Control.Lens (view)
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
  , cmdEndRenderingKHR, pattern IMAGE_LAYOUT_ATTACHMENT_OPTIMAL_KHR, pattern IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL_KHR, Extent2D, cmdDraw
  )
import Vulkan.Zero
import qualified Data.Vector as V

import Vulkan.CStruct.Extends (SomeStruct(..))
import Linear (V4(..))
import Hickory.Vulkan.Textures (transitionImageLayout)
import Hickory.Vulkan.Vulkan (DeviceContext (..), VulkanResources (..), Swapchain (..), allocate, ViewableImage (..))
import Data.Generics.Labels ()
import Hickory.Vulkan.Material (Material, cmdBindMaterial)
import Hickory.Vulkan.Framing (FramedResource, resourceForFrame)
import Hickory.Vulkan.OffscreenTarget (OffscreenTarget)
import Control.Arrow ((&&&))

-- |Contains resources needed to render a frame. Need two of these for 'Double Buffering'.
data Frame = Frame
  { imageAvailableSemaphore :: Semaphore
  , renderFinishedSemaphore :: Semaphore
  , inFlightFence           :: Fence
  , commandPool             :: CommandPool
  , commandBuffer           :: CommandBuffer
  }

-- |User accessible context to render a given frame
data FrameContext = FrameContext
  { extent        :: Extent2D      -- swapchain extent
  , colorImage    :: ViewableImage -- swapchain color image for this frame (to be presented on screen)
  , depthImage    :: ViewableImage -- depth image
  , commandBuffer :: CommandBuffer -- commandbuffer to render this frame
  , frameNumber   :: Int           -- used to index FramedResources
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

useDynamicRenderPass :: MonadIO m => CommandBuffer -> Extent2D -> V4 Float -> ViewableImage -> ViewableImage -> IO () -> m ()
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
    , depthAttachment = Just $ zero
      { imageView   = view #imageView depthImage
      , imageLayout = IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL_KHR
      , loadOp      = ATTACHMENT_LOAD_OP_CLEAR
      , storeOp     = ATTACHMENT_STORE_OP_STORE
      , clearValue  = DepthStencil (ClearDepthStencilValue 1 0)
      }
    }
  liftIO f
  cmdEndRenderingKHR commandBuffer

singlePass :: MonadIO m => V4 Float -> FrameContext -> IO () -> m ()
singlePass clearColor FrameContext {..} f = do
  transitionImageLayout (view #image colorImage) IMAGE_LAYOUT_UNDEFINED IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL commandBuffer
  transitionImageLayout (view #image depthImage) IMAGE_LAYOUT_UNDEFINED IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL commandBuffer

  useDynamicRenderPass commandBuffer extent clearColor colorImage depthImage f

  transitionImageLayout (view #image colorImage) IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL IMAGE_LAYOUT_PRESENT_SRC_KHR commandBuffer

twoPass :: MonadIO m => V4 Float -> FrameContext -> Material -> FramedResource OffscreenTarget -> IO () -> m ()
twoPass clearColor FrameContext {..} postMaterial offscreenTarget mainPass = do
  let (offscreenColor, offscreenDepth) = (view #colorImage &&& view #depthImage) (resourceForFrame frameNumber offscreenTarget)
  -- transition swap images
  transitionImageLayout (view #image colorImage) IMAGE_LAYOUT_UNDEFINED IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL commandBuffer
  transitionImageLayout (view #image depthImage) IMAGE_LAYOUT_UNDEFINED IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL commandBuffer

  -- transition offscreen images
  transitionImageLayout (view #image offscreenColor) IMAGE_LAYOUT_UNDEFINED IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL commandBuffer
  transitionImageLayout (view #image offscreenDepth) IMAGE_LAYOUT_UNDEFINED IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL commandBuffer

  -- render offscreen
  useDynamicRenderPass commandBuffer extent clearColor offscreenColor offscreenDepth mainPass

  -- prepare offscreen image for use as shader input
  transitionImageLayout (view #image offscreenColor) IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL commandBuffer
  transitionImageLayout (view #image offscreenDepth) IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL commandBuffer

  -- render to swap images
  useDynamicRenderPass commandBuffer extent clearColor colorImage depthImage do
    cmdBindMaterial frameNumber commandBuffer postMaterial
    cmdDraw commandBuffer 3 1 0 0

  -- prepare swap image for presentation
  transitionImageLayout (view #image colorImage) IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL IMAGE_LAYOUT_PRESENT_SRC_KHR commandBuffer

{- Drawing a frame -}
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

        liftIO $ f (FrameContext extent image depthImage commandBuffer frameNumber)

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
