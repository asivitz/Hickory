{-# LANGUAGE BlockArguments, ScopedTypeVariables, RecordWildCards, PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE DataKinds, OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}
{-# HLINT ignore "Redundant <&>" #-}

module Hickory.Vulkan.Vulkan where

import Control.Monad
import Control.Monad.Managed
import Vulkan
  ( ColorSpaceKHR (COLOR_SPACE_SRGB_NONLINEAR_KHR)
  , CompositeAlphaFlagBitsKHR (..)
  , Device (..)
  , DeviceCreateInfo(..)
  , DeviceQueueCreateInfo(..)
  , ExtensionProperties (..)
  , Extent2D (..)
  , Format (..)
  , ImageAspectFlagBits (..)
  , ImageSubresourceRange(..)
  , ImageUsageFlagBits (..)
  , ImageViewCreateInfo(..)
  , ImageViewType (IMAGE_VIEW_TYPE_2D)
  , Instance (..)
  , PhysicalDevice
  , PhysicalDeviceProperties (..)
  , PhysicalDeviceType (..)
  , PresentModeKHR (..)
  , Queue
  , QueueFlagBits (..)
  , RenderPass
  , SharingMode (..)
  , SurfaceCapabilitiesKHR(..)
  , SurfaceFormatKHR(..)
  , SurfaceKHR
  , SwapchainCreateInfoKHR(..)
  , SwapchainKHR
  , enumerateDeviceExtensionProperties
  , enumeratePhysicalDevices
  , getDeviceQueue
  , getPhysicalDeviceProperties
  , getPhysicalDeviceQueueFamilyProperties
  , getPhysicalDeviceSurfaceCapabilitiesKHR
  , getPhysicalDeviceSurfaceFormatsKHR
  , getPhysicalDeviceSurfaceSupportKHR
  , getSwapchainImagesKHR
  , pattern KHR_PORTABILITY_SUBSET_EXTENSION_NAME
  , pattern KHR_SWAPCHAIN_EXTENSION_NAME
  , queueFlags
  , withDevice
  , withImageView
  , withSwapchainKHR
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
  , withCommandPool
  , CommandPoolCreateInfo(..), CommandPoolCreateFlagBits (..), CommandBufferAllocateInfo(..), CommandBufferLevel (..), withCommandBuffers
  , withSemaphore
  , withFence
  , FenceCreateInfo(..)
  , FenceCreateFlagBits (..)
  , CommandBuffer(..)
  , Semaphore
  , Fence
  , Image
  , CommandPool
  , physicalDeviceHandle
  , deviceHandle
  , instanceHandle
  , pattern API_VERSION_1_0
  )
import Control.Exception (bracket)
import Vulkan.Zero
import qualified Data.Vector as V
import Data.Ord (comparing)

import Data.Word (Word32)
import Vulkan.Utils.Misc ((.&&.))
import Data.Functor ((<&>))
import Vulkan.CStruct.Extends (SomeStruct(..))
import Data.List (nub)
import Control.Applicative ((<|>))
import Data.Traversable (for)
import VulkanMemoryAllocator hiding (getPhysicalDeviceProperties)
import qualified Vulkan.Dynamic as VD
import Foreign (castFunPtr)

data Bag = Bag
  { deviceContext         :: DeviceContext
  , swapchain             :: Swapchain
  , renderpass            :: RenderPass
  , framebuffers          :: V.Vector Framebuffer
  , frames                :: V.Vector Frame
  , allocator             :: Allocator
  , shortLivedCommandPool :: CommandPool -- For, e.g., mem copy commands
  }

-- |Contains resources needed to render a frame. Need two of these for 'Double Buffering'.
data Frame = Frame
  { imageAvailableSemaphore :: Semaphore
  , renderFinishedSemaphore :: Semaphore
  , inFlightFence           :: Fence
  , commandPool             :: CommandPool
  , commandBuffer           :: CommandBuffer
  }

data DeviceContext = DeviceContext
  { device            :: Device
  , surfaceFormat     :: SurfaceFormatKHR
  , graphicsQueue     :: Queue
  , presentQueue      :: Queue
  , graphicsFamilyIdx :: Word32
  , presentFamilyIdx  :: Word32
  , physicalDevice    :: PhysicalDevice
  , presentMode       :: PresentModeKHR
  }

data Swapchain = Swapchain
  { imageFormat       :: SurfaceFormatKHR
  , swapchainHandle   :: SwapchainKHR
  -- , imageCount        :: Int
  , images            :: V.Vector Image
  , imageViews        :: V.Vector ImageView
  , extent            :: Extent2D
  }

allocate :: IO a -> (a -> IO ()) -> Managed a
allocate c d = managed (bracket c d)

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

{- DEVICE CREATION -}

selectPhysicalDevice :: MonadIO m => Instance -> SurfaceKHR -> m (PhysicalDevice, SurfaceFormatKHR, PresentModeKHR, Word32, Word32)
selectPhysicalDevice inst surface = do
  (_, devices)      <- enumeratePhysicalDevices inst
  elaboratedDevices <- V.mapMaybeM elaborateDevice devices

  pure . project . V.maximumBy (comparing rank) $ elaboratedDevices
  where
  elaborateDevice :: MonadIO m => PhysicalDevice -> m (Maybe (PhysicalDevice, PhysicalDeviceProperties, SurfaceFormatKHR, PresentModeKHR, Word32, Word32))
  elaborateDevice dev = do
    deviceProperties   <- getPhysicalDeviceProperties dev
    graphicsQueueIndex <- getGraphicsQueueIdx dev
    presentQueueIndex  <- getPresentQueueIdx dev
    format             <- getSurfaceFormat dev
    -- This is supposed to be always available
    -- If we want something else, like PRESENT_MODE_MAILBOX_KHR, we need to query for it
    let presentMode    = PRESENT_MODE_FIFO_KHR

    (_, extensions) <- enumerateDeviceExtensionProperties dev Nothing
    let hasExtension x = V.any ((==x) . extensionName) extensions

    pure $ guard (hasExtension KHR_SWAPCHAIN_EXTENSION_NAME) >>
      (,,,,,) <$> pure dev
              <*> pure deviceProperties
              <*> pure format
              <*> pure presentMode
              <*> graphicsQueueIndex
              <*> presentQueueIndex
  project (dev, _props, format, presentMode, graphQIdx, presentQIdx) = (dev, format, presentMode, graphQIdx, presentQIdx)

  vheadMay v = if V.null v then Nothing else Just (V.head v)

  rank :: (PhysicalDevice, PhysicalDeviceProperties, SurfaceFormatKHR, PresentModeKHR, Word32, Word32) -> Int
  rank (_dev, props, _format, _presentMode, _graphQIdx, _presentQIdx) = case deviceType props of
    PHYSICAL_DEVICE_TYPE_DISCRETE_GPU   -> 5
    PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU -> 4
    PHYSICAL_DEVICE_TYPE_CPU            -> 3
    PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU    -> 2
    PHYSICAL_DEVICE_TYPE_OTHER          -> 1

  getGraphicsQueueIdx :: MonadIO m => PhysicalDevice -> m (Maybe Word32)
  getGraphicsQueueIdx dev = do
    getPhysicalDeviceQueueFamilyProperties dev <&>
        fmap (fromIntegral . fst)
      . vheadMay
      . V.filter (\(_i,x) -> QUEUE_GRAPHICS_BIT .&&. queueFlags x)
      . V.indexed

  getPresentQueueIdx :: MonadIO m => PhysicalDevice -> m (Maybe Word32)
  getPresentQueueIdx dev = do
    getPhysicalDeviceQueueFamilyProperties dev
      <&> V.indexed
      >>= V.filterM (\(i, _props) -> getPhysicalDeviceSurfaceSupportKHR dev (fromIntegral i) surface)
      <&> fmap (fromIntegral . fst)
        . vheadMay

  getSurfaceFormat :: MonadIO m => PhysicalDevice -> m SurfaceFormatKHR
  getSurfaceFormat dev = do
    (_, formats) <- getPhysicalDeviceSurfaceFormatsKHR dev surface
    maybe (error "No surface formats available") pure
        $ V.find (\SurfaceFormatKHR { format, colorSpace} -> format == FORMAT_B8G8R8A8_SRGB && colorSpace == COLOR_SPACE_SRGB_NONLINEAR_KHR) formats
      <|> vheadMay formats


withLogicalDevice :: Instance -> SurfaceKHR -> Managed DeviceContext
withLogicalDevice inst surface = do
  (physicalDevice, surfaceFormat, presentMode, graphicsFamilyIdx, presentFamilyIdx) <- selectPhysicalDevice inst surface

  (_, extensions) <- enumerateDeviceExtensionProperties physicalDevice Nothing

  let
    deviceCreateInfo :: DeviceCreateInfo '[]
    deviceCreateInfo = zero
      { queueCreateInfos  = V.fromList $ nub [graphicsFamilyIdx, presentFamilyIdx] <&> \idx ->
          SomeStruct $ zero { queueFamilyIndex = idx, queuePriorities = V.fromList [1] }
      , enabledExtensionNames = V.map extensionName
                              $ V.filter (\ExtensionProperties {..} -> extensionName == KHR_SWAPCHAIN_EXTENSION_NAME
                                                                    || extensionName == KHR_PORTABILITY_SUBSET_EXTENSION_NAME
                                         ) extensions
      }

  device <- withDevice physicalDevice deviceCreateInfo Nothing allocate

  graphicsQueue <- getDeviceQueue device graphicsFamilyIdx 0
  presentQueue  <- getDeviceQueue device presentFamilyIdx 0

  pure $ DeviceContext {..}

withSwapchain :: DeviceContext -> SurfaceKHR -> (Int, Int) -> Managed Swapchain
withSwapchain dc@DeviceContext{..} surface (fbWidth, fbHeight) = do
  capabilities <- getPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice surface

  let
    swapchainCreateInfo :: SwapchainCreateInfoKHR '[]
    swapchainCreateInfo = zero
      { surface            = surface
      , minImageCount      = case capabilities of
        SurfaceCapabilitiesKHR {..} -> if maxImageCount > 0 then min maxImageCount (minImageCount + 1) else minImageCount + 1
      , imageFormat        = format (surfaceFormat :: SurfaceFormatKHR)
      , imageColorSpace    = colorSpace surfaceFormat
      , imageExtent        = extent
      , imageArrayLayers   = 1
      , imageUsage         = IMAGE_USAGE_COLOR_ATTACHMENT_BIT
      , imageSharingMode   = imageSharingMode
      , queueFamilyIndices = queueFamilyIndices
      , preTransform       = currentTransform capabilities
      , compositeAlpha     = COMPOSITE_ALPHA_OPAQUE_BIT_KHR
      , presentMode        = presentMode
      , clipped            = True
      }

    (imageSharingMode, queueFamilyIndices) = if graphicsQueue == presentQueue
      then ( SHARING_MODE_EXCLUSIVE , [])
      else ( SHARING_MODE_CONCURRENT, [graphicsFamilyIdx, presentFamilyIdx]
      )
    clamp x min' max' = max (min x max') min'
    extent = case capabilities of
      SurfaceCapabilitiesKHR {..} ->
        if width (currentExtent :: Extent2D) /= maxBound
        then currentExtent
        else Extent2D (clamp (fromIntegral fbWidth) (width (minImageExtent :: Extent2D)) (width (maxImageExtent :: Extent2D)))
                      (clamp (fromIntegral fbHeight) (height (minImageExtent :: Extent2D)) (height (maxImageExtent :: Extent2D)))

  swapchainHandle <- withSwapchainKHR device swapchainCreateInfo Nothing allocate
  let imageFormat = surfaceFormat

  (_, images) <- getSwapchainImagesKHR device swapchainHandle
  imageViews <- for images $ with2DImageView dc (Vulkan.format (surfaceFormat :: SurfaceFormatKHR))

  pure $ Swapchain {..}

withStandardRenderPass' :: Device -> Format -> Managed RenderPass
withStandardRenderPass' dev swapchainImageFormat =
  withRenderPass dev zero
    { attachments  = [attachmentDescription]
    , subpasses    = [subpass]
    , dependencies = [subpassDependency]
    } Nothing allocate
  where
  attachmentDescription :: AttachmentDescription
  attachmentDescription = zero
    { format         = swapchainImageFormat
    , samples        = SAMPLE_COUNT_1_BIT
    , loadOp         = ATTACHMENT_LOAD_OP_CLEAR
    , storeOp        = ATTACHMENT_STORE_OP_STORE
    , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
    , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
    , initialLayout  = IMAGE_LAYOUT_UNDEFINED
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

createFramebuffer :: Device -> RenderPass -> Extent2D -> ImageView -> Managed Framebuffer
createFramebuffer dev renderPass swapchainExtent imageView =
  let framebufferCreateInfo :: FramebufferCreateInfo '[]
      framebufferCreateInfo = zero
        { renderPass  = renderPass
        , attachments = [imageView]
        , width       = width (swapchainExtent :: Extent2D)
        , height      = height (swapchainExtent :: Extent2D)
        , layers      = 1
        }
  in withFramebuffer dev framebufferCreateInfo Nothing allocate

withStandardAllocator :: Instance -> PhysicalDevice -> Device -> Managed Allocator
withStandardAllocator inst physicalDevice device = withAllocator allocInfo allocate
  where
  allocInfo = zero
    { physicalDevice   = physicalDeviceHandle physicalDevice
    , device           = deviceHandle device
    , instance'        = instanceHandle inst
    , vulkanApiVersion = API_VERSION_1_0
    , vulkanFunctions  = Just $ vmaVulkanFunctions inst device
    }

vmaVulkanFunctions :: Instance -> Device -> VulkanFunctions
vmaVulkanFunctions Instance { instanceCmds } Device { deviceCmds } = zero
  { vkGetInstanceProcAddr = castFunPtr $ VD.pVkGetInstanceProcAddr instanceCmds
  , vkGetDeviceProcAddr   = castFunPtr $ VD.pVkGetDeviceProcAddr deviceCmds
  }

with2DImageView :: DeviceContext -> Format -> Image -> Managed ImageView
with2DImageView DeviceContext { device } format image =
  withImageView device imageViewCreateInfo Nothing allocate
  where
  imageViewCreateInfo = zero
    { image      = image
    , viewType   = IMAGE_VIEW_TYPE_2D
    , format     = format
    , components = zero
    , subresourceRange = zero
      { aspectMask     = IMAGE_ASPECT_COLOR_BIT
      , baseMipLevel   = 0
      , levelCount     = 1
      , baseArrayLayer = 0
      , layerCount     = 1
      }
    }
