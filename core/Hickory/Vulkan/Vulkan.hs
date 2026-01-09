{-# LANGUAGE BlockArguments, ScopedTypeVariables, RecordWildCards, PatternSynonyms, DuplicateRecordFields, OverloadedRecordDot #-}
{-# LANGUAGE DataKinds, OverloadedLists, CPP #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}
{-# HLINT ignore "Redundant <&>" #-}
{-# HLINT ignore "Use infix" #-}
{-# LANGUAGE MultiWayIf #-}

module Hickory.Vulkan.Vulkan where

#ifdef DEBUG
import Vulkan (setDebugUtilsObjectNameEXT, DebugUtilsObjectNameInfoEXT (..), objectTypeAndHandle)
#endif

import Vulkan
  ( HasObjectType
  , Device (..)
  , DeviceCreateInfo(..)
  , DeviceQueueCreateInfo(..)
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

  , QueueFlagBits (..)
  , SharingMode (..)
  , SurfaceCapabilitiesKHR(..)
  , SurfaceFormatKHR(..)

  , getDeviceQueue
  , getPhysicalDeviceProperties
  , getPhysicalDeviceQueueFamilyProperties
  , getPhysicalDeviceSurfaceCapabilitiesKHR
  , getPhysicalDeviceSurfaceFormatsKHR
  , getPhysicalDeviceSurfaceSupportKHR
  , pattern KHR_PORTABILITY_SUBSET_EXTENSION_NAME
  , queueFlags
  , withDevice
  , withImageView
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
  , PhysicalDeviceDescriptorIndexingFeatures (..), ImageCreateInfo(..), ImageType (..), Extent3D (..), ImageTiling (..), MemoryPropertyFlagBits (..), ImageAspectFlags
  , PhysicalDeviceScalarBlockLayoutFeatures(..), framebufferColorSampleCounts, PhysicalDevicePortabilitySubsetFeaturesKHR(..), depthClamp, samplerAnisotropy, independentBlend, getPhysicalDeviceFeatures, PhysicalDeviceVulkan13Features(..), pattern KHR_PORTABILITY_SUBSET_SPEC_VERSION, withSemaphore
  )
import Vulkan.Zero
import qualified Data.Vector as V

import Data.Word (Word32)
import Vulkan.Utils.Misc ((.&&.))
import Data.Functor ((<&>))
import Vulkan.CStruct.Extends (SomeStruct(..), extendSomeStruct, withSomeStruct)
import Data.List (nub)
import Control.Applicative ((<|>))
import Data.Traversable (for)
import VulkanMemoryAllocator hiding (getPhysicalDeviceProperties)
import qualified Vulkan.Dynamic as VD
import Foreign (castFunPtr)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Acquire (Acquire (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (for_)
import Data.Bits ((.|.), (.&.), zeroBits)
import Hickory.Vulkan.Types (DeviceContext(..), Swapchain (..), VulkanResources (..), ViewableImage (..))
import System.Info (os)
import Data.ByteString (ByteString)

import Vulkan.Utils.Initialization (pickPhysicalDevice)
import Vulkan.Utils.Requirements (checkDeviceRequirements, prettyRequirementResult, RequirementResult (..))
import Vulkan.Requirement (DeviceRequirement(..))

import Vulkan.Extensions.VK_EXT_full_screen_exclusive as FSE
import Vulkan.Extensions.VK_KHR_swapchain as SC


{- DEVICE CREATION -}

selectPhysicalDevice :: MonadIO m => Instance -> SurfaceKHR -> m (Maybe ((SurfaceFormatKHR, PresentModeKHR, Word32, Word32, SampleCountFlagBits, PhysicalDeviceProperties, SomeStruct DeviceCreateInfo, Bool), PhysicalDevice))
selectPhysicalDevice inst surface = pickPhysicalDevice inst suitability scoring
  where
  scoring (_surfFormat, _presMode, _, _, _maxSampleCount, props, _, _) = case deviceType props of
    PHYSICAL_DEVICE_TYPE_DISCRETE_GPU   -> 5 :: Int
    PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU -> 4
    PHYSICAL_DEVICE_TYPE_CPU            -> 3
    PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU    -> 2
    PHYSICAL_DEVICE_TYPE_OTHER          -> 1
  vheadMay v = if V.null v then Nothing else Just (V.head v)
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
  suitability dev = do

    deviceProperties   <- getPhysicalDeviceProperties dev
    _deviceFeatures    <- getPhysicalDeviceFeatures dev
    graphicsQueueIndex <- getGraphicsQueueIdx dev
    presentQueueIndex  <- getPresentQueueIdx dev
    format             <- getSurfaceFormat dev
    -- This is supposed to be always available
    -- If we want something else, like PRESENT_MODE_MAILBOX_KHR, we need to query for it
    let presentMode    = PRESENT_MODE_FIFO_KHR
    let maxSampleCount = case framebufferColorSampleCounts . limits $ deviceProperties of
          c | c .&&. SAMPLE_COUNT_64_BIT -> SAMPLE_COUNT_64_BIT
          c | c .&&. SAMPLE_COUNT_32_BIT -> SAMPLE_COUNT_32_BIT
          c | c .&&. SAMPLE_COUNT_16_BIT -> SAMPLE_COUNT_16_BIT
          c | c .&&. SAMPLE_COUNT_8_BIT  -> SAMPLE_COUNT_8_BIT
          c | c .&&. SAMPLE_COUNT_4_BIT  -> SAMPLE_COUNT_4_BIT
          c | c .&&. SAMPLE_COUNT_2_BIT  -> SAMPLE_COUNT_2_BIT
          _                              -> SAMPLE_COUNT_1_BIT

    case (graphicsQueueIndex, presentQueueIndex) of
      (Just graphicsQueueIdx, Just presentQueueIdx) -> do
        let deviceCreateInfo :: DeviceCreateInfo '[]
            deviceCreateInfo = zero
              { queueCreateInfos  = V.fromList $ nub [graphicsQueueIdx, presentQueueIdx] <&> \idx ->
                  SomeStruct $ zero { queueFamilyIndex = idx, queuePriorities = V.fromList [1] }
              , enabledFeatures = Just $ zero { depthClamp = True, samplerAnisotropy = True, independentBlend = True }
              }
            reqRequests :: [DeviceRequirement]
            reqRequests =
              (if os == "darwin"
               then (
                 [ RequireDeviceExtension { deviceExtensionLayerName = Nothing
                                          , deviceExtensionName = KHR_PORTABILITY_SUBSET_EXTENSION_NAME
                                          , deviceExtensionMinVersion = KHR_PORTABILITY_SUBSET_SPEC_VERSION
                                          }
                   -- Needed for for comparison samplers, e.g. sampler2DShadow, but only in conjunction w/ portability subset
                 , RequireDeviceFeature { featureName = "MUTABLE COMPARISON SAMPLERS"
                                        , checkFeature = \s -> s.mutableComparisonSamplers
                                        , enableFeature = \s -> s { mutableComparisonSamplers = True }
                                        }
                 ]++)
               else id
              )
              [ RequireDeviceExtension { deviceExtensionLayerName = Nothing
                                       , deviceExtensionName       = KHR_SWAPCHAIN_EXTENSION_NAME
                                       , deviceExtensionMinVersion = KHR_SWAPCHAIN_SPEC_VERSION
                                       }
              -- Can start render passes without making Render Pass and Framebuffer objects
              , RequireDeviceFeature { featureName = "DYNAMIC RENDERING"
                                     , checkFeature = \s -> s.dynamicRendering
                                     , enableFeature = \s -> s { dynamicRendering = True }
                                     }
              -- Needed for global texture array (b/c has unknown size) ,
              , RequireDeviceFeature { featureName = "RUNTIME DESCRIPTOR ARRAY"
                                     , checkFeature = \s -> s.runtimeDescriptorArray
                                     , enableFeature = \s -> s { runtimeDescriptorArray = True }
                                     }
              -- Can use scalar block layout (tight packing) in shaders
              , RequireDeviceFeature { featureName = "SCALAR BLOCK LAYOUT"
                                     , checkFeature = \s -> s.scalarBlockLayout
                                     , enableFeature = \s -> s { scalarBlockLayout = True }
                                     }
              ]

            optRequests :: [DeviceRequirement]
            optRequests = [
              -- Can we disallow Full Screen Exclusive mode (which seems to crash on windows)
              RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                                     , deviceExtensionName       = EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME
                                     , deviceExtensionMinVersion = EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION
                                     }
              ]

        (mDeviceCreateInfoRes, reqResults, optResults) <- checkDeviceRequirements reqRequests optRequests dev deviceCreateInfo
        let [fseResult] = optResults
            fullScreenExclusiveExtEnabled = case fseResult of
              Satisfied -> True
              _ -> False

        liftIO do
          for_ reqResults \res -> do
            case res of
              Satisfied -> pure ()
              a -> putStrLn $ prettyRequirementResult a
          for_ optResults \res -> do
            case res of
              Satisfied -> pure ()
              a -> putStrLn $ prettyRequirementResult a

        case mDeviceCreateInfoRes of
          Nothing -> pure Nothing
          Just deviceCreateInfoRes -> do
            pure $
              (,,,,,,,) <$> pure format
                      <*> pure presentMode
                      <*> graphicsQueueIndex
                      <*> presentQueueIndex
                      <*> pure maxSampleCount
                      <*> pure deviceProperties
                      <*> pure deviceCreateInfoRes
                      <*> pure fullScreenExclusiveExtEnabled
      _ -> pure Nothing

withLogicalDevice :: Instance -> SurfaceKHR -> Acquire DeviceContext
withLogicalDevice inst surface = do
  mRes <- selectPhysicalDevice inst surface
  case mRes of
    Nothing -> liftIO $ ioError (userError "No acceptable device found")
    Just ((surfaceFormat, presentMode, graphicsFamilyIdx, presentFamilyIdx, maxSampleCount, properties, deviceCreateInfo, fullScreenExclusiveExtEnabled), physicalDevice) -> do

      case deviceCreateInfo of
        SomeStruct ss -> do
          device <- withDevice physicalDevice ss Nothing mkAcquire

          graphicsQueue <- getDeviceQueue device graphicsFamilyIdx 0
          presentQueue  <- getDeviceQueue device presentFamilyIdx 0

          pure $ DeviceContext {..}

withSwapchain :: DeviceContext -> SurfaceKHR -> (Int, Int) -> Acquire (Maybe Swapchain)
withSwapchain dc@DeviceContext{..} surface (fbWidth, fbHeight) = do
  capabilities <- getPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice surface

  let
    compAlpha = case capabilities of
      SurfaceCapabilitiesKHR {..} ->
        if | supportedCompositeAlpha .&. COMPOSITE_ALPHA_OPAQUE_BIT_KHR /= zeroBits -> COMPOSITE_ALPHA_OPAQUE_BIT_KHR
           | supportedCompositeAlpha .&. COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR /= zeroBits -> COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR
           | supportedCompositeAlpha .&. COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR /= zeroBits -> COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR
           | otherwise -> COMPOSITE_ALPHA_INHERIT_BIT_KHR

    swapchainCreateInfoBase :: SomeStruct SwapchainCreateInfoKHR
    swapchainCreateInfoBase = SomeStruct $ zero
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
      , compositeAlpha     = compAlpha
      , presentMode        = presentMode
      , clipped            = True
      }

    -- We want to use this extension to disallow surface from going into Full Screen Exclusive mode on windows
    -- (b/c it seems to crash, and perhaps it's also deprecated in the windows world?)
    fseInfo = SurfaceFullScreenExclusiveInfoEXT { FSE.fullScreenExclusive = FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT }
    swapchainCreateInfo :: SomeStruct SC.SwapchainCreateInfoKHR
    swapchainCreateInfo =
      if fullScreenExclusiveExtEnabled
      then extendSomeStruct fseInfo swapchainCreateInfoBase
      else swapchainCreateInfoBase

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

  case extent of
    Extent2D w h | w == 0 || h == 0 -> pure Nothing
    _ -> do
      swapchainHandle <- withSomeStruct swapchainCreateInfo \sci -> withSwapchainKHR device sci Nothing mkAcquire
      let imageFormat = surfaceFormat

      (_, rawImages) <- getSwapchainImagesKHR device swapchainHandle
      images <- for rawImages $ \image -> do
        let form = surfaceFormat.format
        imageView <- with2DImageView dc form IMAGE_ASPECT_COLOR_BIT image IMAGE_VIEW_TYPE_2D 0 1

        debugName device imageView (BC.pack "SwapchainImageView")
        debugName device image (BC.pack "SwapchainImage")

        renderFinishedSemaphore <- withSemaphore device zero Nothing mkAcquire
        pure (ViewableImage image imageView form, renderFinishedSemaphore)

      pure . Just $ Swapchain {..}

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

with2DImageView :: DeviceContext -> Format -> ImageAspectFlags -> Image -> ImageViewType -> Word32 -> Word32 -> Acquire ImageView
with2DImageView dc format flags image imageViewType = with2DImageViewMips dc format flags image 1 imageViewType

with2DImageViewMips :: DeviceContext -> Format -> ImageAspectFlags -> Image -> Word32 -> ImageViewType -> Word32 -> Word32 -> Acquire ImageView
with2DImageViewMips DeviceContext { device } format flags image mipLevels viewType baseLayer numLayers =
  withImageView device imageViewCreateInfo Nothing mkAcquire
  where
  imageViewCreateInfo = zero
    { image      = image
    , viewType   = viewType
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

createVertShader :: Device -> String -> B.ByteString -> Acquire (SomeStruct PipelineShaderStageCreateInfo)
createVertShader = createShader SHADER_STAGE_VERTEX_BIT

createFragShader :: Device -> String -> B.ByteString -> Acquire (SomeStruct PipelineShaderStageCreateInfo)
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

createShader :: ShaderStageFlagBits -> Device -> String -> B.ByteString -> Acquire (SomeStruct PipelineShaderStageCreateInfo)
createShader stage dev name source = do
  shaderModule <- withShaderModule dev zero { code = source } Nothing mkAcquire
  debugName dev shaderModule (BC.pack name)

  pure . SomeStruct $ zero
    { stage = stage
    , module' = shaderModule
    , name = "main"
    }

debugName :: (MonadIO io, HasObjectType p) => Device -> p -> ByteString -> io ()
debugName dev a name =
#ifdef DEBUG
  let (otype, handle) = objectTypeAndHandle a
  in setDebugUtilsObjectNameEXT dev (DebugUtilsObjectNameInfoEXT otype handle (Just name))
#else
  pure ()
#endif
