{-# LANGUAGE PatternSynonyms, OverloadedLists #-}

module Hickory.Vulkan.Utils where

import Hickory.Vulkan.Vulkan
import Control.Monad
import Vulkan
  ( Instance
  , SurfaceKHR
  , pattern KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
  , deviceWaitIdle, pattern KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME
  )
import Hickory.Types (Size (..))
import Hickory.Vulkan.Framing (resourceForFrame, FramedResource (..), frameResource)
import Hickory.Vulkan.Frame (drawFrame, FrameContext, Frame (..), withFrame)
import Data.Vector (Vector)
import Data.ByteString (ByteString)
import Data.IORef (newIORef, atomicModifyIORef, readIORef, IORef, writeIORef)
import Acquire.Acquire (Acquire)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Vector as V
import Hickory.Vulkan.Instance (withStandardInstance, withVulkanResources)

initVulkan :: Vector ByteString -> (Instance -> Acquire SurfaceKHR) -> Acquire (VulkanResources, FramedResource Frame, (Int,Int) -> Acquire Swapchain)
initVulkan extensions surfCreate = do
  inst            <- withStandardInstance $ extensions V.++ [ "VK_EXT_debug_utils"
                                                            , KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                                                            , KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME -- required by MoltenVK
                                                            ]
  surface         <- surfCreate inst
  vulkanResources <- withVulkanResources inst surface
  frames          <- frameResource $ withFrame (deviceContext vulkanResources)
  pure (vulkanResources, frames, withSwapchain vulkanResources surface)

buildFrameFunction
  :: Vector ByteString -- ^ Extensions
  -> IO (Size Int) -- ^ Query framebuffer size
  -> (Instance -> Acquire SurfaceKHR)
  -> (Size Int -> VulkanResources -> Swapchain -> Acquire userRes) -- ^ Acquire user resources
  -> (userRes -> FrameContext -> IO ()) -- ^ Run with frame context
  -> Acquire (IO (), IO ()) -- ^ (Execute a frame, Cleanup user res)
buildFrameFunction extensions queryFbSize acqSurface acqUserRes exeFrame = do
  (vulkanResources, frames, swapchainCreate) <- initVulkan extensions acqSurface
  frameCounter :: IORef Int <- liftIO $ newIORef 0

  -- When the window is resized, we have to rebuild the swapchain
  -- Any resources that depend on the swapchain need to be rebuilt as well
  let
    acquireDynamicResources = do
      (Size w h) <- liftIO queryFbSize
      swapchain <- swapchainCreate (w,h)
      userResources <- acqUserRes (Size w h) vulkanResources swapchain
      pure (swapchain, userResources)

  dynamicResources <- liftIO $ unWrapAcquire acquireDynamicResources >>= newIORef

  let
    waitForIdleDevice = deviceWaitIdle (device (deviceContext vulkanResources))
    runASingleFrame = do
      frameNumber <- atomicModifyIORef frameCounter (\a -> (a+1,a+1))
      let frame = resourceForFrame frameNumber frames
      ((swapchain, userResources), releaseRes) <- liftIO $ readIORef dynamicResources
      drawRes <- drawFrame frameNumber frame vulkanResources swapchain (exeFrame userResources)
      unless drawRes do
        waitForIdleDevice
        releaseRes
        unWrapAcquire acquireDynamicResources >>= writeIORef dynamicResources
    cleanup = do
      waitForIdleDevice
      readIORef dynamicResources >>= snd
  pure (runASingleFrame, cleanup)
