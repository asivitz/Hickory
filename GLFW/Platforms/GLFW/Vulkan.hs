{-# LANGUAGE BlockArguments, LambdaCase, ScopedTypeVariables, RecordWildCards, PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE DataKinds, OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}
{-# HLINT ignore "Redundant <&>" #-}

module Platforms.GLFW.Vulkan where

import Hickory.Vulkan.Vulkan
import qualified Graphics.UI.GLFW as GLFW
import Control.Monad
import Control.Monad.Managed
import Vulkan
  ( pattern API_VERSION_1_0
  , ApplicationInfo(..)
  , Instance
  , InstanceCreateInfo (..)
  , SurfaceFormatKHR(..)
  , SurfaceKHR
  , destroySurfaceKHR
  , instanceHandle
  , pattern KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
  , withInstance, CommandPoolCreateInfo(..), withCommandPool, CommandPoolCreateFlagBits (..)
  , CommandBuffer, deviceWaitIdle
  )
import Foreign (alloca, nullPtr, peek)
import Vulkan.Zero
import qualified Data.Vector as V
import qualified Data.ByteString as B
import Data.Traversable (for)

{- GLFW -}

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
  GLFW.setErrorCallback $ Just simpleErrorCallback
  r <- GLFW.init
  GLFW.windowHint (GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI)

  when r do
    m <- GLFW.createWindow width height title Nothing Nothing
    case m of
      (Just win) -> do
          GLFW.setErrorCallback $ Just simpleErrorCallback

          f win

          GLFW.destroyWindow win
          GLFW.terminate

      Nothing -> do
          print ("ERROR: Couldn't create window" :: String)
  where
  simpleErrorCallback e s = putStrLn $ unwords [show e, show s]

withVulkanResources :: Instance -> SurfaceKHR -> Managed VulkanResources
withVulkanResources inst surface = do
  deviceContext@DeviceContext {..} <- withLogicalDevice inst surface
  allocator <- withStandardAllocator inst physicalDevice device
  shortLivedCommandPool <-
    let commandPoolCreateInfo :: CommandPoolCreateInfo
        commandPoolCreateInfo = zero { queueFamilyIndex = graphicsFamilyIdx, flags = COMMAND_POOL_CREATE_TRANSIENT_BIT }
    in withCommandPool device commandPoolCreateInfo Nothing allocate
  pure VulkanResources {..}

withSwapchainContext :: SurfaceKHR -> VulkanResources -> (Int, Int) -> Managed SwapchainContext
withSwapchainContext surface VulkanResources {..} framebufferSize = do
  let DeviceContext {..} = deviceContext
  swapchain@Swapchain {..} <- withSwapchain deviceContext surface framebufferSize

  renderpass   <- withStandardRenderPass' device (format (surfaceFormat :: SurfaceFormatKHR))
  framebuffers <- for imageViews $ createFramebuffer device renderpass extent
  pure SwapchainContext {..}

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

validationLayers :: V.Vector B.ByteString
validationLayers = V.fromList ["VK_LAYER_KHRONOS_validation"]

runFrames
  :: GLFW.Window
  -> (VulkanResources -> SwapchainContext -> Managed userRes) -- ^ Acquire user resources
  -> (VulkanResources -> SwapchainContext -> Managed perFrameUserRes) -- ^ Acquire user resources that are needed 1 copy per frame (e.g. for double buffering)
  -> (userRes -> perFrameUserRes -> CommandBuffer -> IO ()) -- ^ Execute a frame
  -> IO ()
runFrames win acquireUserResources acquirePerFrameUserResources f = do
  glfwReqExts <- GLFW.getRequiredInstanceExtensions >>= fmap V.fromList . mapM B.packCString

  runManaged do
    let numFrames = 2
    inst            <- withStandardInstance $ glfwReqExts V.++ [ "VK_EXT_debug_utils",  KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME ]
    surface         <- withWindowSurface inst win
    vulkanResources <- withVulkanResources inst surface
    frames          <- V.replicateM numFrames $ withFrame (deviceContext vulkanResources)

    -- When the window is resized, we have to rebuild the swapchain
    -- Any resources that depend on the swapchain need to be rebuilt as well
    let acquireDynamicResources = do
          framebufferSize       <- liftIO $ GLFW.getFramebufferSize win
          swapchainContext      <- withSwapchainContext surface vulkanResources framebufferSize
          userResources         <- acquireUserResources vulkanResources swapchainContext
          perFrameUserResources <- V.replicateM numFrames $ acquirePerFrameUserResources vulkanResources swapchainContext
          pure (swapchainContext, userResources, perFrameUserResources)

    liftIO $ loopWithResourceRefresh acquireDynamicResources \frameNumber (swapchainContext, userResources, perFrameUserResources) -> do
      GLFW.pollEvents
      let frame = frames V.! (frameNumber `mod` V.length frames)
          thisFrameUserResources = perFrameUserResources V.! (frameNumber `mod` V.length perFrameUserResources)

      drawRes <- drawFrame frame vulkanResources swapchainContext (f userResources thisFrameUserResources)
      shouldClose <- GLFW.windowShouldClose win
      when (drawRes || shouldClose) $ deviceWaitIdle (device (deviceContext vulkanResources))
      if shouldClose then pure Nothing else pure (Just drawRes)

-- |Acquire resources and loop. User function may signal:
-- Nothing    -> Exit loop
-- Just False -> Require resources and loop
-- Just True  -> Loop
loopWithResourceRefresh :: Managed a -> (Int -> a -> IO (Maybe Bool)) -> IO ()
loopWithResourceRefresh acquireResource f = main 0
  where
  main n = do
    outer n >>= \case
      Just n' -> void $ main (n' + 1)
      Nothing -> pure ()
  outer n = do
    with acquireResource \resource -> do
      let inner n' = do
            f n' resource >>= \case
              Just True  -> inner (n' + 1)
              Just False -> pure (Just n')
              Nothing    -> pure Nothing
      inner n
