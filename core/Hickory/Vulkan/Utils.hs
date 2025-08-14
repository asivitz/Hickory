{-# LANGUAGE PatternSynonyms, OverloadedLists #-}

module Hickory.Vulkan.Utils where

import Hickory.Vulkan.Vulkan
import Control.Monad
import Vulkan
  ( Instance
  , SurfaceKHR
  , pattern KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
  , pattern KHR_SURFACE_EXTENSION_NAME
  , deviceWaitIdle, pattern KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME, CommandPoolCreateInfo(..), withCommandPool, CommandPoolCreateFlagBits (..)
  )
import Hickory.Types (Size (..))
import Hickory.Vulkan.Framing (resourceForFrame, frameResource)
import Hickory.Vulkan.Frame (drawFrame, withFrame)
import Data.ByteString (ByteString)
import Data.IORef (newIORef, atomicModifyIORef, readIORef, IORef, writeIORef)
import Acquire (Acquire (..))
import Control.Monad.IO.Class (liftIO)
import Hickory.Vulkan.Instance (withStandardInstance, validationLayers)
import Vulkan.Zero (zero)
import Hickory.Vulkan.Types (DeviceContext(..), VulkanResources (..), Swapchain, FrameContext)
import Control.Concurrent (threadDelay)

initVulkan :: [ByteString] -> (Instance -> Acquire SurfaceKHR) -> Acquire VulkanResources
initVulkan extensions surfCreate = do
  let defaultExtensions = [ "VK_EXT_debug_utils"
                          , KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                          , KHR_SURFACE_EXTENSION_NAME
                          , KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME -- required by MoltenVK
                          ]
  inst            <- withStandardInstance (extensions ++ defaultExtensions) validationLayers
  surface         <- surfCreate inst

  deviceContext@DeviceContext {..} <- withLogicalDevice inst surface
  allocator <- withStandardAllocator inst physicalDevice device
  shortLivedCommandPool <-
    let commandPoolCreateInfo :: CommandPoolCreateInfo
        commandPoolCreateInfo = zero { queueFamilyIndex = graphicsFamilyIdx, flags = COMMAND_POOL_CREATE_TRANSIENT_BIT }
    in withCommandPool device commandPoolCreateInfo Nothing mkAcquire


  frames <- frameResource $ withFrame deviceContext

  cleanupQueue <- mkAcquire (newIORef $ replicate 3 (pure ())) (readIORef >=> sequence_)

  let acquireSwapchain = withSwapchain deviceContext surface
  pure VulkanResources {..}

buildFrameFunction
  :: VulkanResources
  -> IO (Size Int) -- ^ Query framebuffer size
  -> (Swapchain -> Acquire userRes) -- ^ Acquire user resources
  -> Acquire ((userRes -> FrameContext -> IO ()) -> IO ()) -- ^ Execute a frame
buildFrameFunction vulkanResources@VulkanResources {..} queryFbSize acqUserRes = do
  frameCounter :: IORef Int <- liftIO $ newIORef 0

  -- When the window is resized, we have to rebuild the swapchain
  -- Any resources that depend on the swapchain need to be rebuilt as well
  let
    acquireDynamicResources = do
      Size w h <- liftIO queryFbSize
      acquireSwapchain (w,h) >>= \case
        Just swapchain -> do
          userResources <- acqUserRes swapchain
          pure $ Just (swapchain, userResources)
        Nothing -> pure Nothing

  dynamicResources <- liftIO $ unWrapAcquire acquireDynamicResources >>= newIORef

  let
    waitForIdleDevice = deviceWaitIdle (device deviceContext)
    runASingleFrame exeFrame = do
      frameNumber <- atomicModifyIORef frameCounter (\a -> (a+1,a+1))
      let frame = resourceForFrame (fromIntegral frameNumber) frames -- usually we index by swapchainImageIndex, but we don't have it yet
      (mRes, releaseRes) <- liftIO $ readIORef dynamicResources
      case mRes of
        Just (swapchain, userResources) -> do
          drawRes <- drawFrame frameNumber frame vulkanResources swapchain (exeFrame userResources)
          unless drawRes do
            waitForIdleDevice
            releaseRes
            unWrapAcquire acquireDynamicResources >>= writeIORef dynamicResources
        Nothing -> do
          liftIO $ threadDelay 1000 -- 1 ms                                             ..
          waitForIdleDevice
          releaseRes
          unWrapAcquire acquireDynamicResources >>= writeIORef dynamicResources
    cleanup = do
      waitForIdleDevice
      readIORef dynamicResources >>= snd
  Acquire $ pure (runASingleFrame, cleanup)
