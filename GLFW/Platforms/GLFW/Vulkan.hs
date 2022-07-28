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
  ( ApplicationInfo(..)
  , Instance
  , InstanceCreateInfo (..)
  , SurfaceKHR
  , destroySurfaceKHR
  , instanceHandle
  , pattern KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
  , withInstance, CommandPoolCreateInfo(..), withCommandPool, CommandPoolCreateFlagBits (..)
  , CommandBuffer, deviceWaitIdle, pattern KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME, pattern API_VERSION_1_2, InstanceCreateFlagBits (..), Extent2D
  )
import Foreign (alloca, nullPtr, peek)
import Vulkan.Zero
import qualified Data.Vector as V
import qualified Data.ByteString as B
import Hickory.Types (Size (..))
import Hickory.Vulkan.Framing (frameResource, resourceForFrame)
import Hickory.Vulkan.Frame (withFrame, drawFrame, FrameContext)

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

withStandardInstance :: V.Vector B.ByteString -> Managed Instance
withStandardInstance extensions = withInstance instanceCreateInfo Nothing allocate
  where
  instanceCreateInfo = zero
    { applicationInfo = Just zero { applicationName = Just "Vulkan Demo", apiVersion = API_VERSION_1_2 }
    , enabledLayerNames     = validationLayers
    , enabledExtensionNames = extensions
    , flags = INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR
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
  -> (Size Int -> VulkanResources -> Swapchain -> Managed userRes) -- ^ Acquire user resources
  -> (userRes -> FrameContext -> IO ()) -- ^ Execute a frame
  -> IO ()
runFrames win acquireUserResources f = do
  glfwReqExts <- GLFW.getRequiredInstanceExtensions >>= fmap V.fromList . mapM B.packCString

  runManaged do
    inst            <- withStandardInstance $ glfwReqExts V.++ [ "VK_EXT_debug_utils"
                                                               , KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                                                               , KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME -- required by MoltenVK
                                                               ]
    surface         <- withWindowSurface inst win
    vulkanResources <- withVulkanResources inst surface
    frames          <- frameResource $ withFrame (deviceContext vulkanResources)

    -- When the window is resized, we have to rebuild the swapchain
    -- Any resources that depend on the swapchain need to be rebuilt as well
    let acquireDynamicResources = do
          (w,h) <- liftIO $ GLFW.getFramebufferSize win
          let fbSize = Size w h
          swapchain <- withSwapchain vulkanResources surface (w,h)
          userResources <- acquireUserResources fbSize vulkanResources swapchain
          pure (swapchain, userResources)

    liftIO $ loopWithResourceRefresh acquireDynamicResources \frameNumber (swapchainContext, userResources) -> do
      GLFW.pollEvents
      let frame = resourceForFrame frameNumber frames

      drawRes <- drawFrame frameNumber frame vulkanResources swapchainContext (f userResources)
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
