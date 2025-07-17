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
import Data.ByteString (ByteString)
import Data.IORef (newIORef, atomicModifyIORef, readIORef, IORef, writeIORef)
import Acquire (Acquire (..))
import Control.Monad.IO.Class (liftIO)
import Hickory.Vulkan.Instance (withStandardInstance, validationLayers)
import Vulkan.Zero (zero)
import Hickory.Vulkan.Types (DeviceContext(..), VulkanResources (..), Swapchain, FrameContext)
