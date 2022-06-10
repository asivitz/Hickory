{-# LANGUAGE BlockArguments, LambdaCase, ScopedTypeVariables, RecordWildCards, PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE DataKinds, OverloadedLists, QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}
{-# HLINT ignore "Redundant <&>" #-}

module Main where

import qualified Graphics.UI.GLFW as GLFW
import Control.Monad
import Control.Monad.Managed
import Vulkan
  ( pattern API_VERSION_1_0
  , ApplicationInfo(..)
  , ColorSpaceKHR (COLOR_SPACE_SRGB_NONLINEAR_KHR)
  , ComponentMapping(..)
  , ComponentSwizzle (..)
  , CompositeAlphaFlagBitsKHR (..)
  , Device
  , DeviceCreateInfo
  , DeviceCreateInfo(..)
  , DeviceQueueCreateInfo(..)
  , ExtensionProperties (..)
  , Extent2D (..)
  , Format (..)
  , GraphicsPipelineCreateInfo(..)
  , ImageAspectFlagBits (..)
  , ImageSubresourceRange(..)
  , ImageUsageFlagBits (..)
  , ImageViewCreateInfo(..)
  , ImageViewType (IMAGE_VIEW_TYPE_2D)
  , Instance
  , InstanceCreateInfo (..)
  , PhysicalDevice
  , PhysicalDeviceProperties (..)
  , PhysicalDeviceType (..)
  , Pipeline
  , PipelineInputAssemblyStateCreateInfo(..)
  , PipelineShaderStageCreateInfo(..)
  , PresentModeKHR (..)
  , Queue
  , QueueFlagBits (..)
  , RenderPass
  , ShaderModuleCreateInfo(..)
  , ShaderStageFlagBits (..)
  , SharingMode (..)
  , SurfaceCapabilitiesKHR(..)
  , SurfaceFormatKHR(..)
  , SurfaceKHR
  , SwapchainCreateInfoKHR(..)
  , SwapchainKHR
  , colorSpace
  , destroySurfaceKHR
  , enumerateDeviceExtensionProperties
  , enumeratePhysicalDevices
  , extensionName
  , format
  , getDeviceQueue
  , getPhysicalDeviceProperties
  , getPhysicalDeviceQueueFamilyProperties
  , getPhysicalDeviceSurfaceCapabilitiesKHR
  , getPhysicalDeviceSurfaceFormatsKHR
  , getPhysicalDeviceSurfaceSupportKHR
  , getSwapchainImagesKHR
  , instanceHandle
  , pattern KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
  , pattern KHR_PORTABILITY_SUBSET_EXTENSION_NAME
  , pattern KHR_SWAPCHAIN_EXTENSION_NAME
  , queueFlags
  , withDevice
  , withImageView
  , withInstance
  , withShaderModule
  , withSwapchainKHR, Viewport (..)
  , withRenderPass
  , PipelineViewportStateCreateInfo(..), Rect2D (..)
  , PipelineRasterizationStateCreateInfo(..)
  , PipelineMultisampleStateCreateInfo(..)
  , PipelineColorBlendAttachmentState(..)
  , PipelineColorBlendStateCreateInfo(..), PrimitiveTopology (..), Offset2D (..), withPipelineLayout, PolygonMode (..), CullModeFlagBits (..), FrontFace (..), SampleCountFlagBits (..), ColorComponentFlagBits (..), withGraphicsPipelines, AttachmentDescription(..), SubpassDescription(..), SubpassDependency(..)
  , RenderPassCreateInfo(..)
  , AttachmentReference(..), AttachmentLoadOp (..), AttachmentStoreOp (..), ImageLayout (..), PipelineBindPoint (..), pattern SUBPASS_EXTERNAL, PipelineStageFlagBits (..), AccessFlagBits (..)
  , FramebufferCreateInfo(..), ImageView
  , Framebuffer, withFramebuffer, withCommandPool
  , CommandPoolCreateInfo(..), CommandPoolCreateFlagBits (..), CommandBufferAllocateInfo(..), CommandBufferLevel (..), withCommandBuffers
  , RenderPassBeginInfo(..), useCommandBuffer, ClearValue (..), ClearColorValue (..), cmdUseRenderPass, SubpassContents (..), cmdBindPipeline, cmdDraw, withSemaphore, withFence, waitForFences, resetFences
  , FenceCreateInfo(..), FenceCreateFlagBits (..), acquireNextImageKHR, CommandBuffer(..), resetCommandBuffer
  , SubmitInfo(..)
  , PresentInfoKHR(..), queueSubmit, queuePresentKHR, deviceWaitIdle, Semaphore, Fence
  )
import qualified Vulkan
import Foreign (alloca, nullPtr, peek, Bits ((.|.)))
import Control.Exception (bracket)
import Vulkan.Zero
import qualified Data.Vector as V
import Data.Ord (comparing)

import qualified Data.ByteString as B
import Data.Word (Word32)
import Vulkan.Utils.Misc ((.&&.))
import Data.Functor ((<&>))
import Vulkan.CStruct.Extends (SomeStruct(..))
import Data.List (nub)
import Control.Applicative ((<|>))
import Data.Traversable (for)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag, vert)
import Data.Foldable (for_)
import Control.Monad.Loops (whileM_)

data Bag = Bag
  { renderpass     :: RenderPass
  , pipeline       :: Pipeline
  , commandBuffers :: V.Vector CommandBuffer
  , framebuffers   :: V.Vector Framebuffer
  , deviceContext  :: DeviceContext
  }

data DeviceContext = DeviceContext
  { device            :: Device
  , swapchain         :: SwapchainKHR
  , format            :: Format
  , swapchainExtent   :: Extent2D
  , graphicsQueue     :: Queue
  , presentQueue      :: Queue
  , graphicsFamilyIdx :: Word32
  , presentFamilyIdx  :: Word32
  }

main :: IO ()
main = withWindow 800 800 "Vulkan Test" $ \win Bag{..} -> do
  let DeviceContext {..} = deviceContext
  runManaged do
    imageAvailableSemaphore <- withSemaphore device zero Nothing allocate
    renderFinishedSemaphore <- withSemaphore device zero Nothing allocate
    inFlightFence           <- withFence device zero { flags = FENCE_CREATE_SIGNALED_BIT } Nothing allocate

    whileM_ (not <$> liftIO (GLFW.windowShouldClose win)) do
      liftIO GLFW.pollEvents

      _ <- waitForFences device [ inFlightFence ] True maxBound
      resetFences device [ inFlightFence ]

      (_, imageIndex) <- acquireNextImageKHR device swapchain maxBound imageAvailableSemaphore zero

      for_ commandBuffers $ \x -> resetCommandBuffer x zero

      liftIO . for_ (V.zip framebuffers commandBuffers) $ \(framebuffer, cmdBuffer) -> useCommandBuffer cmdBuffer zero do
        let renderPassBeginInfo = zero
              { renderPass  = renderpass
              , framebuffer = framebuffer
              , renderArea  = Rect2D { offset = zero , extent = swapchainExtent }
              , clearValues = [ Color (Float32 0.0 0.0 0.0 1.0) ]
              }
        cmdUseRenderPass cmdBuffer renderPassBeginInfo SUBPASS_CONTENTS_INLINE do
          cmdBindPipeline cmdBuffer PIPELINE_BIND_POINT_GRAPHICS pipeline
          cmdDraw cmdBuffer 3 1 0 0

      let submitInfo = zero
            { waitSemaphores   = [imageAvailableSemaphore]
            , waitDstStageMask = [PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
            , commandBuffers   = [commandBufferHandle $ commandBuffers V.! fromIntegral imageIndex]
            , signalSemaphores = [renderFinishedSemaphore]
            }
      queueSubmit graphicsQueue [SomeStruct submitInfo] inFlightFence
      _ <- queuePresentKHR presentQueue $ zero
        { waitSemaphores = [renderFinishedSemaphore]
        , swapchains     = [swapchain]
        , imageIndices   = [imageIndex]
        }
      pure ()

    deviceWaitIdle device

validationLayers :: V.Vector B.ByteString
validationLayers = V.fromList ["VK_LAYER_KHRONOS_validation"]

allocate :: IO a -> (a -> IO ()) -> Managed a
allocate c d = managed (bracket c d)

{- GLFW -}

withWindow :: Int -> Int -> String -> (GLFW.Window -> Bag -> IO ()) -> IO ()
withWindow width height title f = do
  GLFW.setErrorCallback $ Just simpleErrorCallback
  r <- GLFW.init
  GLFW.windowHint (GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI)
  GLFW.windowHint (GLFW.WindowHint'Resizable False)

  when r do
    m <- GLFW.createWindow width height title Nothing Nothing
    case m of
      (Just win) -> do
          GLFW.setErrorCallback $ Just simpleErrorCallback

          glfwReqExts <- GLFW.getRequiredInstanceExtensions >>= fmap V.fromList . mapM B.packCString

          framebufferSize <- GLFW.getFramebufferSize win

          runManaged do
            inst    <- withStandardInstance $ glfwReqExts V.++ [ "VK_EXT_debug_utils",  KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME ]
            surface <- withWindowSurface inst win
            deviceContext@DeviceContext {..} <- withLogicalDevice inst surface framebufferSize

            (_, images) <- getSwapchainImagesKHR device swapchain
            imageViews <- for images \x ->
              let imageViewCreateInfo = zero
                    { image            = x
                    , viewType         = IMAGE_VIEW_TYPE_2D
                    , format           = format
                    , components       = zero { r = COMPONENT_SWIZZLE_IDENTITY
                                              , g = COMPONENT_SWIZZLE_IDENTITY
                                              , b = COMPONENT_SWIZZLE_IDENTITY
                                              , a = COMPONENT_SWIZZLE_IDENTITY
                                              }
                    , subresourceRange = zero { aspectMask     = IMAGE_ASPECT_COLOR_BIT
                                              , baseMipLevel   = 0
                                              , levelCount     = 1
                                              , baseArrayLayer = 0
                                              , layerCount     = 1
                                              }
                    }
              in withImageView device imageViewCreateInfo Nothing allocate

            renderpass   <- withStandardRenderPass' device format
            pipeline     <- withGraphicsPipeline device renderpass swapchainExtent
            framebuffers <- for imageViews $ createFramebuffer device renderpass swapchainExtent

            commandPool <-
              let commandPoolCreateInfo :: CommandPoolCreateInfo
                  commandPoolCreateInfo = zero { queueFamilyIndex = graphicsFamilyIdx, flags = COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT }
              in withCommandPool device commandPoolCreateInfo Nothing allocate

            commandBuffers <-
              let commandBufferAllocateInfo :: CommandBufferAllocateInfo
                  commandBufferAllocateInfo = zero
                    { commandPool        = commandPool
                    , level              = COMMAND_BUFFER_LEVEL_PRIMARY
                    , commandBufferCount = fromIntegral $ V.length framebuffers
                    }
              in withCommandBuffers device commandBufferAllocateInfo allocate

            liftIO $ f win Bag {..}

          GLFW.destroyWindow win
          GLFW.terminate

      Nothing -> do
          print ("ERROR: Couldn't create window" :: String)
  where
  simpleErrorCallback e s = putStrLn $ unwords [show e, show s]

withStandardInstance :: V.Vector B.ByteString -> Managed Instance
withStandardInstance extensions = withInstance instanceCreateInfo Nothing allocate
  where
  instanceCreateInfo = zero
    { applicationInfo = Just zero { applicationName = Just "Vulkan Demo", apiVersion = API_VERSION_1_0 }
    , enabledLayerNames     = validationLayers
    , enabledExtensionNames = extensions
    }

withWindowSurface :: Instance -> GLFW.Window -> Managed SurfaceKHR
withWindowSurface inst window = allocate create release
  where
  create = alloca \ptr ->
    GLFW.createWindowSurface (instanceHandle inst) window nullPtr ptr >>= \case
      (0 :: Int) -> peek ptr
      res        -> error $ "Error when creating window surface: " ++ show res
  release surf = destroySurfaceKHR inst surf Nothing

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


withLogicalDevice :: Instance -> SurfaceKHR -> (Int, Int) -> Managed DeviceContext
withLogicalDevice inst surface (fbWidth, fbHeight) = do
  (physicalDevice, surfaceFormat, presentMode, graphicsFamilyIdx, presentFamilyIdx) <- selectPhysicalDevice inst surface

  (_, extensions) <- enumerateDeviceExtensionProperties physicalDevice Nothing

  let
    deviceCreateInfo :: DeviceCreateInfo '[]
    deviceCreateInfo = zero
      { enabledLayerNames = validationLayers
      , queueCreateInfos  = V.fromList $ nub [graphicsFamilyIdx, presentFamilyIdx] <&> \idx ->
          SomeStruct $ zero { queueFamilyIndex = idx, queuePriorities = V.fromList [1] }
      , enabledExtensionNames = V.map extensionName
                              $ V.filter (\ExtensionProperties {..} -> extensionName == KHR_SWAPCHAIN_EXTENSION_NAME
                                                                    || extensionName == KHR_PORTABILITY_SUBSET_EXTENSION_NAME
                                         ) extensions
      }

  device <- withDevice physicalDevice deviceCreateInfo Nothing allocate

  graphicsQueue <- getDeviceQueue device graphicsFamilyIdx 0
  presentQueue  <- getDeviceQueue device presentFamilyIdx 0

  capabilities <- getPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice surface

  let
    swapchainCreateInfo :: SwapchainCreateInfoKHR '[]
    swapchainCreateInfo = zero
      { surface            = surface
      , minImageCount      = case capabilities of
        SurfaceCapabilitiesKHR {..} -> if maxImageCount > 0 then min maxImageCount (minImageCount + 1) else minImageCount + 1
      , imageFormat        = format (surfaceFormat :: SurfaceFormatKHR)
      , imageColorSpace    = colorSpace surfaceFormat
      , imageExtent        = swapchainExtent
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
    swapchainExtent = case capabilities of
      SurfaceCapabilitiesKHR {..} ->
        if width (currentExtent :: Extent2D) /= maxBound
        then currentExtent
        else Extent2D (clamp (fromIntegral fbWidth) (width (minImageExtent :: Extent2D)) (width (maxImageExtent :: Extent2D)))
                      (clamp (fromIntegral fbHeight) (height (minImageExtent :: Extent2D)) (height (maxImageExtent :: Extent2D)))

  swapchain <- withSwapchainKHR device swapchainCreateInfo Nothing allocate
  let format = Vulkan.format (surfaceFormat :: SurfaceFormatKHR)

  pure $ DeviceContext {..}

{- GRAPHICS PIPELINE -}

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

withGraphicsPipeline :: Device -> RenderPass -> Extent2D -> Managed Pipeline
withGraphicsPipeline dev renderPass swapchainExtent = do
  shaderStages   <- sequence [ createVertShader dev vertShader, createFragShader dev fragShader ]
  pipelineLayout <- withPipelineLayout dev zero Nothing allocate

  let
    pipelineCreateInfo :: GraphicsPipelineCreateInfo '[]
    pipelineCreateInfo = zero
      { stages             = shaderStages
      , vertexInputState   = Just zero
      , inputAssemblyState = Just zero
          { topology = PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
          , primitiveRestartEnable = False
          }
      , viewportState = Just . SomeStruct $ zero
        { viewports =
          [ Viewport
              { x        = 0
              , y        = 0
              , width    = realToFrac $ width  (swapchainExtent :: Extent2D)
              , height   = realToFrac $ height (swapchainExtent :: Extent2D)
              , minDepth = 0
              , maxDepth = 1
              }
          ]
        , scissors  = [ Rect2D { offset = Offset2D 0 0, extent = swapchainExtent } ]
        }
      , rasterizationState = Just . SomeStruct $ zero
          { depthClampEnable        = False
          , rasterizerDiscardEnable = False
          , polygonMode             = POLYGON_MODE_FILL
          , lineWidth               = 1
          , cullMode                = CULL_MODE_BACK_BIT
          , frontFace               = FRONT_FACE_CLOCKWISE
          , depthBiasEnable         = False
          }
      , multisampleState = Just . SomeStruct $ zero
          { sampleShadingEnable  = False
          , rasterizationSamples = SAMPLE_COUNT_1_BIT
          }
      , depthStencilState = Nothing
      , colorBlendState = Just . SomeStruct $ zero
          { logicOpEnable = False
          , attachments =
            [ zero
              { colorWriteMask
                =   COLOR_COMPONENT_R_BIT
                .|. COLOR_COMPONENT_G_BIT
                .|. COLOR_COMPONENT_B_BIT
                .|. COLOR_COMPONENT_A_BIT
              , blendEnable = False
              }
            ]
          }
      , dynamicState       = Nothing
      , layout             = pipelineLayout
      , renderPass         = renderPass
      , subpass            = 0
      , basePipelineHandle = zero
      }
  V.head . snd
    <$> withGraphicsPipelines dev zero [SomeStruct pipelineCreateInfo] Nothing allocate

{-- SHADERS --}

createVertShader :: Device -> B.ByteString -> Managed (SomeStruct PipelineShaderStageCreateInfo)
createVertShader = createShader SHADER_STAGE_VERTEX_BIT

createFragShader :: Device -> B.ByteString -> Managed (SomeStruct PipelineShaderStageCreateInfo)
createFragShader = createShader SHADER_STAGE_FRAGMENT_BIT

createShader :: ShaderStageFlagBits -> Device -> B.ByteString -> Managed (SomeStruct PipelineShaderStageCreateInfo)
createShader stage dev source = do
  shaderModule <- withShaderModule dev zero { code = source } Nothing allocate
  pure . SomeStruct $ zero
    { stage = stage
    , module' = shaderModule
    , name = "main"
    }

vertShader :: B.ByteString
vertShader = [vert|
  #version 450

  layout(location = 0) out vec3 fragColor;

  vec3 colors[3] = vec3[](
      vec3(1.0, 0.0, 0.0),
      vec3(0.0, 1.0, 0.0),
      vec3(0.0, 0.0, 1.0)
  );

  vec2 positions[3] = vec2[](
      vec2(0.0, -0.5),
      vec2(0.5, 0.5),
      vec2(-0.5, 0.5)
  );


  void main() {
      gl_Position = vec4(positions[gl_VertexIndex], 0.0, 1.0);
      fragColor = colors[gl_VertexIndex];
  }

|]

fragShader :: B.ByteString
fragShader = [frag|
  #version 450

  layout(location = 0) in vec3 fragColor;
  layout(location = 0) out vec4 outColor;

  void main() {
    outColor = vec4(fragColor, 1.0);
  }

|]
