{-# LANGUAGE BlockArguments, ScopedTypeVariables, RecordWildCards, PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE DataKinds, OverloadedLists, DeriveGeneric #-}
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
  , ImageViewType (IMAGE_VIEW_TYPE_2D)
  , Instance (..)
  , PhysicalDevice
  , PhysicalDeviceProperties (..)
  , PhysicalDeviceType (..)
  , PresentModeKHR (..)
  , Queue
  , QueueFlagBits (..)
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
  , pattern KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME
  , pattern KHR_CREATE_RENDERPASS_2_EXTENSION_NAME
  , queueFlags
  , withDevice
  , withImageView
  , withSwapchainKHR
  , SampleCountFlagBits (..)
  , ImageLayout (..)
  , ImageView
  , Image
  , CommandPool
  , physicalDeviceHandle
  , deviceHandle
  , instanceHandle
  , pattern API_VERSION_1_2
  , PipelineLayout
  , GraphicsPipelineCreateInfo(..)
  , Pipeline
  , PipelineInputAssemblyStateCreateInfo(..)
  , Viewport (..)
  , PipelineViewportStateCreateInfo(..), Rect2D (..)
  , PipelineRasterizationStateCreateInfo(..)
  , PipelineMultisampleStateCreateInfo(..)
  , PipelineColorBlendAttachmentState(..)
  , PipelineColorBlendStateCreateInfo(..), PrimitiveTopology (..), Offset2D (..), PolygonMode (..), CullModeFlagBits (..), FrontFace (..), ColorComponentFlagBits (..), withGraphicsPipelines
  , PipelineVertexInputStateCreateInfo(..), VertexInputBindingDescription, VertexInputAttributeDescription, PipelineShaderStageCreateInfo
  , ShaderModuleCreateInfo(..)
  , ShaderStageFlagBits (..)
  , withShaderModule
  , PipelineShaderStageCreateInfo(..)
  , BlendOp (..), BlendFactor (..), pattern KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME, pattern EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME, pattern KHR_MAINTENANCE3_EXTENSION_NAME
  , PhysicalDeviceDescriptorIndexingFeatures (..), ImageCreateInfo(..), ImageType (..), Extent3D (..), ImageTiling (..), MemoryPropertyFlagBits (..), ImageAspectFlags
  , PipelineDepthStencilStateCreateInfo(..), CompareOp (..)
  , PipelineRenderingCreateInfo(..), pattern KHR_DYNAMIC_RENDERING_EXTENSION_NAME, PhysicalDeviceDynamicRenderingFeatures(..)
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
import Foreign (castFunPtr, Bits ((.|.)))
import qualified Data.ByteString as B
import GHC.Generics (Generic)
import Acquire.Acquire (Acquire (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.List as DL
import Data.Foldable (for_)

data VulkanResources = VulkanResources
  { deviceContext         :: DeviceContext
  , allocator             :: Allocator
  , shortLivedCommandPool :: CommandPool -- For, e.g., mem copy commands
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
  , images            :: V.Vector ViewableImage
  , extent            :: Extent2D
  , depthFormat       :: Format
  , depthImage        :: ViewableImage
  }

data ViewableImage = ViewableImage
  { image     :: Image
  , imageView :: ImageView
  } deriving Generic

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


withLogicalDevice :: Instance -> SurfaceKHR -> Acquire DeviceContext
withLogicalDevice inst surface = do
  (physicalDevice, surfaceFormat, presentMode, graphicsFamilyIdx, presentFamilyIdx) <- selectPhysicalDevice inst surface

  (_, V.toList . fmap extensionName -> availableExtensions) <- enumerateDeviceExtensionProperties physicalDevice Nothing

  let
    desiredExtensions = [ KHR_SWAPCHAIN_EXTENSION_NAME
                        , KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME
                        , EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME -- Larger descriptor sets (e.g. for global images descriptor set)
                        , KHR_MAINTENANCE3_EXTENSION_NAME -- required for descriptor indexing
                        , KHR_DYNAMIC_RENDERING_EXTENSION_NAME -- new api not needing RenderPasses
                        , KHR_PORTABILITY_SUBSET_EXTENSION_NAME -- required for moltenvk
                        , KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME -- required for the above dynamic rendering extension
                        , KHR_CREATE_RENDERPASS_2_EXTENSION_NAME -- required for the above dynamic rendering extension
                        ]

  let
    extensionsToEnable = DL.intersect desiredExtensions availableExtensions
    extensionsNotAvailable = desiredExtensions DL.\\ extensionsToEnable

    deviceCreateInfo :: DeviceCreateInfo '[PhysicalDeviceDescriptorIndexingFeatures, PhysicalDeviceDynamicRenderingFeatures]
    deviceCreateInfo = zero
      { queueCreateInfos  = V.fromList $ nub [graphicsFamilyIdx, presentFamilyIdx] <&> \idx ->
          SomeStruct $ zero { queueFamilyIndex = idx, queuePriorities = V.fromList [1] }
      , enabledExtensionNames = V.fromList extensionsToEnable
      , next = ( zero { runtimeDescriptorArray = True } -- Needed for global texture array (b/c has unknown size)
               , ( zero { dynamicRendering = True } -- Can start render passes without making Render Pass and Framebuffer objects
               , () )
               )
      }

  for_ extensionsNotAvailable \e ->
    liftIO . putStrLn $ "Device extension not available: " ++ show e

  device <- withDevice physicalDevice deviceCreateInfo Nothing mkAcquire

  graphicsQueue <- getDeviceQueue device graphicsFamilyIdx 0
  presentQueue  <- getDeviceQueue device presentFamilyIdx 0

  pure $ DeviceContext {..}

withSwapchain :: VulkanResources -> SurfaceKHR -> (Int, Int) -> Acquire Swapchain
withSwapchain vr@VulkanResources {..} surface (fbWidth, fbHeight) = do
  let dc@DeviceContext{..} = deviceContext
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

  swapchainHandle <- withSwapchainKHR device swapchainCreateInfo Nothing mkAcquire
  let imageFormat = surfaceFormat

  (_, rawImages) <- getSwapchainImagesKHR device swapchainHandle
  images <- for rawImages $ \image ->
    ViewableImage image <$> with2DImageView dc (Vulkan.format (surfaceFormat :: SurfaceFormatKHR)) IMAGE_ASPECT_COLOR_BIT image

  let depthFormat = FORMAT_D32_SFLOAT
  depthImageRaw  <- withDepthImage vr extent depthFormat
  depthImageView <- with2DImageView dc depthFormat IMAGE_ASPECT_DEPTH_BIT depthImageRaw
  let depthImage = ViewableImage depthImageRaw depthImageView

  pure $ Swapchain {..}

withDepthImage :: VulkanResources -> Extent2D -> Format -> Acquire Image
withDepthImage VulkanResources { allocator } (Extent2D width height) format = do

  let imageCreateInfo :: ImageCreateInfo '[]
      imageCreateInfo = zero
        { imageType     = IMAGE_TYPE_2D
        , extent        = Extent3D (fromIntegral width) (fromIntegral height) 1
        , format        = format
        , mipLevels     = 1
        , arrayLayers   = 1
        , tiling        = IMAGE_TILING_OPTIMAL
        , samples       = SAMPLE_COUNT_1_BIT
        , usage         = IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
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

with2DImageView :: DeviceContext -> Format -> ImageAspectFlags -> Image -> Acquire ImageView
with2DImageView DeviceContext { device } format flags image =
  withImageView device imageViewCreateInfo Nothing mkAcquire
  where
  imageViewCreateInfo = zero
    { image      = image
    , viewType   = IMAGE_VIEW_TYPE_2D
    , format     = format
    , components = zero
    , subresourceRange = zero
      { aspectMask     = flags
      , baseMipLevel   = 0
      , levelCount     = 1
      , baseArrayLayer = 0
      , layerCount     = 1
      }
    }

{- GRAPHICS PIPELINE -}

withGraphicsPipeline
  :: VulkanResources
  -> Swapchain
  -> PrimitiveTopology
  -> B.ByteString
  -> B.ByteString
  -> PipelineLayout
  -> V.Vector VertexInputBindingDescription
  -> V.Vector VertexInputAttributeDescription
  -> Acquire Pipeline
withGraphicsPipeline
  VulkanResources {..} swapchain
  topology vertShader fragShader pipelineLayout vertexBindingDescriptions vertexAttributeDescriptions
  = do
  let DeviceContext {..} = deviceContext
  let Swapchain {..} = swapchain
  shaderStages   <- sequence [ createVertShader device vertShader, createFragShader device fragShader ]

  let
    pipelineCreateInfo :: GraphicsPipelineCreateInfo '[ PipelineRenderingCreateInfo ]
    pipelineCreateInfo = zero
      { stages             = shaderStages
      , vertexInputState   = Just . SomeStruct $ zero
        { vertexBindingDescriptions   = vertexBindingDescriptions
        , vertexAttributeDescriptions = vertexAttributeDescriptions
        }
      , inputAssemblyState = Just zero
          { topology = topology
          , primitiveRestartEnable = False
          }
      , viewportState = Just . SomeStruct $ zero
        { viewports =
          [ Viewport
              { x        = 0
              , y        = 0
              , width    = realToFrac $ width  (extent :: Extent2D)
              , height   = realToFrac $ height (extent :: Extent2D)
              , minDepth = 0
              , maxDepth = 1
              }
          ]
        , scissors  = [ Rect2D { offset = Offset2D 0 0, extent = extent } ]
        }
      , rasterizationState = Just . SomeStruct $ zero
          { depthClampEnable        = False
          , rasterizerDiscardEnable = False
          , polygonMode             = POLYGON_MODE_FILL
          , lineWidth               = 1
          , cullMode                = CULL_MODE_BACK_BIT
          , frontFace               = FRONT_FACE_COUNTER_CLOCKWISE
          , depthBiasEnable         = False
          }
      , multisampleState = Just . SomeStruct $ zero
          { sampleShadingEnable  = False
          , rasterizationSamples = SAMPLE_COUNT_1_BIT
          }
      , depthStencilState = Just $ zero
        { depthTestEnable       = True
        , depthWriteEnable      = True
        , depthCompareOp        = COMPARE_OP_LESS
        , depthBoundsTestEnable = False
        , stencilTestEnable     = False
        }
      , colorBlendState = Just . SomeStruct $ zero
          { logicOpEnable = False
          , attachments =
            [ zero
              { colorWriteMask
                =   COLOR_COMPONENT_R_BIT
                .|. COLOR_COMPONENT_G_BIT
                .|. COLOR_COMPONENT_B_BIT
                .|. COLOR_COMPONENT_A_BIT
              , blendEnable = True
              , srcColorBlendFactor = BLEND_FACTOR_SRC_ALPHA
              , dstColorBlendFactor = BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
              , colorBlendOp = BLEND_OP_ADD
              , srcAlphaBlendFactor = BLEND_FACTOR_ONE
              , dstAlphaBlendFactor = BLEND_FACTOR_ZERO
              , alphaBlendOp = BLEND_OP_ADD
              }
            ]
          }
      , dynamicState       = Nothing
      , layout             = pipelineLayout
      , subpass            = 0
      , basePipelineHandle = zero
      , next = (zero
        { colorAttachmentFormats = [ Vulkan.format (imageFormat :: SurfaceFormatKHR) ]
        , depthAttachmentFormat = depthFormat
        }, ())
      }
  V.head . snd
    <$> withGraphicsPipelines device zero [SomeStruct pipelineCreateInfo] Nothing mkAcquire

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
