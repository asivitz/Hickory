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
  )
import Foreign (alloca, nullPtr, peek)
import qualified Data.ByteString as B
import Hickory.Types (Size (..))
import Hickory.Vulkan.Frame (FrameContext)
import Hickory.Vulkan.Utils (buildFrameFunction)
import Control.Monad.Fix (fix)
import Acquire.Acquire (Acquire)
import Control.Monad.IO.Class (liftIO)
import Platforms.GLFW.DearImGui (initDearImGui, renderDearImGui)
import DearImGui (showDemoWindow, newFrame)
import DearImGui.Vulkan (vulkanNewFrame)
import DearImGui.GLFW (glfwNewFrame)

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
  -> (Size Int -> Instance -> VulkanResources -> Swapchain -> Acquire userRes) -- ^ Acquire user resources
  -> (userRes -> FrameContext -> IO ()) -- ^ Execute a frame
  -> IO ()
runFrames win acquireUserResources f = do
  glfwReqExts <- GLFW.getRequiredInstanceExtensions >>= mapM B.packCString
  runAcquire do
    let imguiAcquire size inst vr swap =
          (,) <$> initDearImGui win inst vr swap
              <*> acquireUserResources size inst vr swap
        imguiRender (imguiRes, userRes) frameContext = do
          renderDearImGui imguiRes frameContext do
            f userRes frameContext

    -- TODO: Option to turn off dear-imgui?
    -- (exeFrame, cleanup) <- buildFrameFunction glfwReqExts (uncurry Size <$> GLFW.getFramebufferSize win) (`withWindowSurface` win) acquireUserResources f
    (exeFrame, cleanup) <- buildFrameFunction glfwReqExts (uncurry Size <$> GLFW.getFramebufferSize win) (`withWindowSurface` win) imguiAcquire imguiRender

    let
      glfwRunFrame = do
        GLFW.pollEvents
        exeFrame
        GLFW.windowShouldClose win

    fix \rec -> liftIO glfwRunFrame >>= \case
      False -> rec
      True -> pure ()
    liftIO cleanup
