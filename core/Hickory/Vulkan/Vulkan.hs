{-# LANGUAGE BlockArguments, ScopedTypeVariables, RecordWildCards, PatternSynonyms, DuplicateRecordFields, OverloadedRecordDot #-}
{-# LANGUAGE DataKinds, OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}
{-# HLINT ignore "Redundant <&>" #-}
{-# HLINT ignore "Use infix" #-}

module Hickory.Vulkan.Vulkan where

import Control.Monad
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
  , ImageViewType (..)
  , Instance (..)
  , PhysicalDevice
  , PhysicalDeviceProperties (..)
  , PhysicalDeviceType (..)
  , PresentModeKHR (..)

  , QueueFlagBits (..)
  , SharingMode (..)
  , SurfaceCapabilitiesKHR(..)
  , SurfaceFormatKHR(..)
  , SurfaceKHR
  , SwapchainCreateInfoKHR(..)

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
  , SampleCountFlagBits (..)
  , ImageLayout (..)
  , ImageView
  , Image

  , physicalDeviceHandle
  , deviceHandle
  , instanceHandle
  , pattern API_VERSION_1_2
  , PipelineShaderStageCreateInfo
  , ShaderModuleCreateInfo(..)
  , ShaderStageFlagBits (..)
  , withShaderModule
  , PipelineShaderStageCreateInfo(..)
  , pattern KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME, pattern EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME, pattern KHR_MAINTENANCE3_EXTENSION_NAME
  , PhysicalDeviceDescriptorIndexingFeatures (..), ImageCreateInfo(..), ImageType (..), Extent3D (..), ImageTiling (..), MemoryPropertyFlagBits (..), ImageAspectFlags
  , PhysicalDeviceDynamicRenderingFeatures(..), framebufferColorSampleCounts, PhysicalDevicePortabilitySubsetFeaturesKHR(..), depthClamp, PhysicalDeviceVulkan12Features, samplerFilterMinmax, samplerAnisotropy, independentBlend
  )
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
import qualified Data.ByteString as B
import Acquire.Acquire (Acquire (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.List as DL
import Data.Foldable (for_)
import Data.Bits ((.|.))
import Hickory.Vulkan.Types (DeviceContext(..), Swapchain (..), VulkanResources (..), ViewableImage (..))

{- DEVICE CREATION -}

selectPhysicalDevice :: MonadIO m => Instance -> SurfaceKHR -> m (PhysicalDevice, SurfaceFormatKHR, PresentModeKHR, Word32, Word32, SampleCountFlagBits, PhysicalDeviceProperties)
selectPhysicalDevice inst surface = do
  (_, devices)      <- enumeratePhysicalDevices inst
  elaboratedDevices <- V.mapMaybeM elaborateDevice devices

  pure . project . V.maximumBy (comparing rank) $ elaboratedDevices
  where
  elaborateDevice :: MonadIO m => PhysicalDevice -> m (Maybe (PhysicalDevice, PhysicalDeviceProperties, SurfaceFormatKHR, PresentModeKHR, Word32, Word32, SampleCountFlagBits))
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

    let maxSampleCount = case framebufferColorSampleCounts . limits $ deviceProperties of
          c | c .&&. SAMPLE_COUNT_64_BIT -> SAMPLE_COUNT_64_BIT
          c | c .&&. SAMPLE_COUNT_32_BIT -> SAMPLE_COUNT_32_BIT
          c | c .&&. SAMPLE_COUNT_16_BIT -> SAMPLE_COUNT_16_BIT
          c | c .&&. SAMPLE_COUNT_8_BIT  -> SAMPLE_COUNT_8_BIT
          c | c .&&. SAMPLE_COUNT_4_BIT  -> SAMPLE_COUNT_4_BIT
          c | c .&&. SAMPLE_COUNT_2_BIT  -> SAMPLE_COUNT_2_BIT
          _                              -> SAMPLE_COUNT_1_BIT

    pure $ guard (hasExtension KHR_SWAPCHAIN_EXTENSION_NAME) >>
      (,,,,,,) <$> pure dev
              <*> pure deviceProperties
              <*> pure format
              <*> pure presentMode
              <*> graphicsQueueIndex
              <*> presentQueueIndex
              <*> pure maxSampleCount
  project (dev, props, format, presentMode, graphQIdx, presentQIdx, maxSampleCount) = (dev, format, presentMode, graphQIdx, presentQIdx, maxSampleCount, props)

  vheadMay v = if V.null v then Nothing else Just (V.head v)

  rank :: (PhysicalDevice, PhysicalDeviceProperties, SurfaceFormatKHR, PresentModeKHR, Word32, Word32, SampleCountFlagBits) -> Int
  rank (_dev, props, _format, _presentMode, _graphQIdx, _presentQIdx, _maxSampleCount) = case deviceType props of
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


withLogicalDevice :: Instance -> SurfaceKHR -> Acquire DeviceContext
withLogicalDevice inst surface = do
  (physicalDevice, surfaceFormat, presentMode, graphicsFamilyIdx, presentFamilyIdx, maxSampleCount, properties) <- selectPhysicalDevice inst surface

  (_, V.toList . fmap extensionName -> availableExtensions) <- enumerateDeviceExtensionProperties physicalDevice Nothing

  let
    desiredExtensions = [ KHR_SWAPCHAIN_EXTENSION_NAME
                        , KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME
                        , EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME -- Larger descriptor sets (e.g. for global images descriptor set)
                        , KHR_MAINTENANCE3_EXTENSION_NAME -- required for descriptor indexing
                        -- , KHR_DYNAMIC_RENDERING_EXTENSION_NAME -- new api not needing RenderPasses
                        , KHR_PORTABILITY_SUBSET_EXTENSION_NAME -- required for moltenvk
                        -- , KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME -- required for the above dynamic rendering extension
                        -- , KHR_CREATE_RENDERPASS_2_EXTENSION_NAME -- required for the above dynamic rendering extension
                        ]

  let
    extensionsToEnable = DL.intersect desiredExtensions availableExtensions
    extensionsNotAvailable = desiredExtensions DL.\\ extensionsToEnable

    deviceCreateInfo :: DeviceCreateInfo '[ PhysicalDeviceDescriptorIndexingFeatures
                                          -- , PhysicalDeviceDynamicRenderingFeatures
                                          , PhysicalDevicePortabilitySubsetFeaturesKHR
                                          ]
    deviceCreateInfo = zero
      { queueCreateInfos  = V.fromList $ nub [graphicsFamilyIdx, presentFamilyIdx] <&> \idx ->
          SomeStruct $ zero { queueFamilyIndex = idx, queuePriorities = V.fromList [1] }
      , enabledExtensionNames = V.fromList extensionsToEnable
      , enabledFeatures = Just $ zero { depthClamp = True, samplerAnisotropy = True, independentBlend = True }
      , next = ( zero { runtimeDescriptorArray = True } -- Needed for global texture array (b/c has unknown size) ,
               -- , (zero { dynamicRendering = True } -- Can start render passes without making Render Pass and Framebuffer objects
               , (zero { mutableComparisonSamplers = True } -- Needed for sampler2DShadow
               , ()
               ))
      }

  for_ extensionsNotAvailable \e ->
    liftIO . putStrLn $ "Device extension not available: " ++ show e

  device <- withDevice physicalDevice deviceCreateInfo Nothing mkAcquire

  graphicsQueue <- getDeviceQueue device graphicsFamilyIdx 0
  presentQueue  <- getDeviceQueue device presentFamilyIdx 0

  pure $ DeviceContext {..}

withSwapchain :: DeviceContext -> SurfaceKHR -> (Int, Int) -> Acquire Swapchain
withSwapchain dc@DeviceContext{..} surface (fbWidth, fbHeight) = do
  capabilities <- getPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice surface

  let
    swapchainCreateInfo :: SwapchainCreateInfoKHR '[]
    swapchainCreateInfo = zero
      { surface            = surface
      , minImageCount      = case capabilities of
        SurfaceCapabilitiesKHR {..} -> if maxImageCount > 0 then min maxImageCount (minImageCount + 1) else minImageCount + 1
      , imageFormat        = surfaceFormat.format
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
        if currentExtent.width /= maxBound
        then currentExtent
        else Extent2D (clamp (fromIntegral fbWidth) (minImageExtent.width) (maxImageExtent.width))
                      (clamp (fromIntegral fbHeight) (minImageExtent.height) (maxImageExtent.height))

  swapchainHandle <- withSwapchainKHR device swapchainCreateInfo Nothing mkAcquire
  let imageFormat = surfaceFormat

  (_, rawImages) <- getSwapchainImagesKHR device swapchainHandle
  images <- for rawImages $ \image -> let form = surfaceFormat.format in
    ViewableImage image <$> with2DImageView dc form IMAGE_ASPECT_COLOR_BIT image 0 1 <*> pure form

  pure $ Swapchain {..}

withDepthImage :: VulkanResources -> Extent2D -> Format -> SampleCountFlagBits -> ImageUsageFlagBits -> Word32 -> Acquire Image
withDepthImage VulkanResources { allocator } (Extent2D width height) format samples usage layers = do

  let imageCreateInfo :: ImageCreateInfo '[]
      imageCreateInfo = zero
        { imageType     = IMAGE_TYPE_2D
        , extent        = Extent3D (fromIntegral width) (fromIntegral height) 1
        , format        = format
        , mipLevels     = 1
        , arrayLayers   = layers
        , tiling        = IMAGE_TILING_OPTIMAL
        , samples       = samples
        , usage         = IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT .|. usage
        , sharingMode   = SHARING_MODE_EXCLUSIVE
        , initialLayout = IMAGE_LAYOUT_UNDEFINED
        }
      allocationCreateInfo :: AllocationCreateInfo
      allocationCreateInfo = zero { requiredFlags = MEMORY_PROPERTY_DEVICE_LOCAL_BIT }

  (image, _, _) <- withImage allocator imageCreateInfo allocationCreateInfo mkAcquire

  pure image

withStandardAllocator :: Instance -> PhysicalDevice -> Device -> Acquire Allocator
withStandardAllocator inst physicalDevice device = withAllocator allocInfo mkAcquire
  where
  allocInfo = zero
    { physicalDevice   = physicalDeviceHandle physicalDevice
    , device           = deviceHandle device
    , instance'        = instanceHandle inst
    , vulkanApiVersion = API_VERSION_1_2
    , vulkanFunctions  = Just $ vmaVulkanFunctions inst device
    }

vmaVulkanFunctions :: Instance -> Device -> VulkanFunctions
vmaVulkanFunctions Instance { instanceCmds } Device { deviceCmds } = zero
  { vkGetInstanceProcAddr = castFunPtr $ VD.pVkGetInstanceProcAddr instanceCmds
  , vkGetDeviceProcAddr   = castFunPtr $ VD.pVkGetDeviceProcAddr deviceCmds
  }

with2DImageView :: DeviceContext -> Format -> ImageAspectFlags -> Image -> Word32 -> Word32 -> Acquire ImageView
with2DImageView dc format flags image = with2DImageViewMips dc format flags image 1

with2DImageViewMips :: DeviceContext -> Format -> ImageAspectFlags -> Image -> Word32 -> Word32 -> Word32 -> Acquire ImageView
with2DImageViewMips DeviceContext { device } format flags image mipLevels baseLayer numLayers =
  withImageView device imageViewCreateInfo Nothing mkAcquire
  where
  imageViewCreateInfo = zero
    { image      = image
    , viewType   = if numLayers > 1 then IMAGE_VIEW_TYPE_2D_ARRAY else IMAGE_VIEW_TYPE_2D
    , format     = format
    , components = zero
    , subresourceRange = zero
      { aspectMask     = flags
      , baseMipLevel   = 0
      , levelCount     = mipLevels
      , baseArrayLayer = baseLayer
      , layerCount     = numLayers
      }
    }

{-- SHADERS --}

createVertShader :: Device -> B.ByteString -> Acquire (SomeStruct PipelineShaderStageCreateInfo)
createVertShader = createShader SHADER_STAGE_VERTEX_BIT

createFragShader :: Device -> B.ByteString -> Acquire (SomeStruct PipelineShaderStageCreateInfo)
createFragShader = createShader SHADER_STAGE_FRAGMENT_BIT

mkAcquire :: IO a -> (a -> IO ()) -> Acquire a
mkAcquire ac rel = Acquire do
  a <- ac
  pure (a, rel a)

runAcquire :: Acquire a -> IO a
runAcquire (Acquire acq) = do
  (a, rel) <- acq
  rel
  pure a

unWrapAcquire :: Acquire a -> IO (a, IO ())
unWrapAcquire (Acquire acq) = acq

createShader :: ShaderStageFlagBits -> Device -> B.ByteString -> Acquire (SomeStruct PipelineShaderStageCreateInfo)
createShader stage dev source = do
  shaderModule <- withShaderModule dev zero { code = source } Nothing mkAcquire

  pure . SomeStruct $ zero
    { stage = stage
    , module' = shaderModule
    , name = "main"
    }
