{-# LANGUAGE BlockArguments, ScopedTypeVariables, RecordWildCards, PatternSynonyms, DuplicateRecordFields #-}

module Hickory.Vulkan.Instance where

import Vulkan
  ( ApplicationInfo(..)
  , Instance
  , InstanceCreateInfo (..)
  , withInstance
  , pattern API_VERSION_1_2, InstanceCreateFlagBits (..), SurfaceKHR, CommandPoolCreateInfo(..), withCommandPool, CommandPoolCreateFlagBits (..)
  )
import Vulkan.Zero
import qualified Data.Vector as V
import qualified Data.ByteString as B
import Hickory.Vulkan.Vulkan (VulkanResources (..), DeviceContext (..), withStandardAllocator, withLogicalDevice, mkAcquire)
import Acquire.Acquire (Acquire)

validationLayers :: V.Vector B.ByteString
validationLayers = V.fromList ["VK_LAYER_KHRONOS_validation"]

withStandardInstance :: V.Vector B.ByteString -> Acquire Instance
withStandardInstance extensions = withInstance instanceCreateInfo Nothing mkAcquire
  where
  instanceCreateInfo = zero
    { applicationInfo = Just zero { applicationName = Just "Vulkan Demo", apiVersion = API_VERSION_1_2 }
    , enabledLayerNames     = validationLayers
    , enabledExtensionNames = extensions
    , flags = INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR
    }

withVulkanResources :: Instance -> SurfaceKHR -> Acquire VulkanResources
withVulkanResources inst surface = do
  deviceContext@DeviceContext {..} <- withLogicalDevice inst surface
  allocator <- withStandardAllocator inst physicalDevice device
  shortLivedCommandPool <-
    let commandPoolCreateInfo :: CommandPoolCreateInfo
        commandPoolCreateInfo = zero { queueFamilyIndex = graphicsFamilyIdx, flags = COMMAND_POOL_CREATE_TRANSIENT_BIT }
    in withCommandPool device commandPoolCreateInfo Nothing mkAcquire
  pure VulkanResources {..}
