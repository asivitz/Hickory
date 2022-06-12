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
  , withInstance
  )
import Foreign (alloca, nullPtr, peek)
import Vulkan.Zero
import qualified Data.Vector as V
import qualified Data.ByteString as B
import Data.Traversable (for)


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
            deviceContext@DeviceContext {..} <- withLogicalDevice inst surface
            swapchain@Swapchain {..} <- withSwapchain deviceContext surface framebufferSize

            renderpass   <- withStandardRenderPass' device (format (surfaceFormat :: SurfaceFormatKHR))
            framebuffers <- for imageViews $ createFramebuffer device renderpass extent

            -- Need 2 frames for double buffering
            frames <- V.replicateM 2 $ withFrame deviceContext

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

validationLayers :: V.Vector B.ByteString
validationLayers = V.fromList ["VK_LAYER_KHRONOS_validation"]

