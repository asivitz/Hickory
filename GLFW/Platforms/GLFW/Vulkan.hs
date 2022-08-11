{-# LANGUAGE BlockArguments, LambdaCase, ScopedTypeVariables, PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE DataKinds, OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}
{-# HLINT ignore "Redundant <&>" #-}

module Platforms.GLFW.Vulkan where

import Hickory.Vulkan.Vulkan
import qualified Graphics.UI.GLFW as GLFW
import Control.Monad
import Vulkan
  ( Instance
  , SurfaceKHR
  , destroySurfaceKHR
  , instanceHandle
  , pattern KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
  , deviceWaitIdle, pattern KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME
  )
import Foreign (alloca, nullPtr, peek)
import qualified Data.Vector as V
import qualified Data.ByteString as B
import Hickory.Types (Size (..))
import Hickory.Vulkan.Framing (frameResource, resourceForFrame, FramedResource)
import Hickory.Vulkan.Frame (withFrame, drawFrame, FrameContext, Frame)
import Hickory.Vulkan.Instance (withStandardInstance, withVulkanResources)
import Hickory.Vulkan.Utils (buildFrameFunction)
import Data.Vector (Vector)
import Data.ByteString (ByteString)
import Data.IORef (newIORef, atomicModifyIORef, readIORef, IORef, writeIORef)
import Control.Monad.Fix (fix)
import Acquire.Acquire (Acquire)
import Control.Monad.IO.Class (liftIO)

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

withWindowSurface :: Instance -> GLFW.Window -> Acquire SurfaceKHR
withWindowSurface inst window = mkAcquire create release
  where
  create = alloca \ptr ->
    GLFW.createWindowSurface (instanceHandle inst) window nullPtr ptr >>= \case
      (0 :: Int) -> peek ptr
      res        -> error $ "Error when creating window surface: " ++ show res
  release surf = destroySurfaceKHR inst surf Nothing

runFrames
  :: GLFW.Window
  -> (Size Int -> VulkanResources -> Swapchain -> Acquire userRes) -- ^ Acquire user resources
  -> (userRes -> FrameContext -> IO ()) -- ^ Execute a frame
  -> IO ()
runFrames win acquireUserResources f = do
  glfwReqExts <- GLFW.getRequiredInstanceExtensions >>= fmap V.fromList . mapM B.packCString
  runAcquire do
    (exeFrame, cleanup) <- buildFrameFunction glfwReqExts (uncurry Size <$> GLFW.getFramebufferSize win) (`withWindowSurface` win) acquireUserResources f

    let
      glfwRunFrame = do
        GLFW.pollEvents
        exeFrame
        GLFW.windowShouldClose win

    fix \rec -> liftIO glfwRunFrame >>= \case
      False -> rec
      True -> pure ()
    liftIO cleanup
