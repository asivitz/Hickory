{-# LANGUAGE BlockArguments, LambdaCase, ScopedTypeVariables, DuplicateRecordFields #-}
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
import Hickory.Vulkan.Utils (buildFrameFunction, initVulkan)
import Control.Monad.Fix (fix)
import Acquire (Acquire)
import Control.Monad.IO.Class (liftIO)
import Platforms.GLFW.DearImGui (initDearImGuiForGLFW)
import Hickory.Vulkan.Types (VulkanResources, FrameContext, Swapchain, runCleanup)
import Hickory.ImGUI.ImGUI (renderDearImGui)
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

initGLFWVulkan :: GLFW.Window -> Acquire VulkanResources
initGLFWVulkan win = do
  glfwReqExts <- liftIO $ GLFW.getRequiredInstanceExtensions >>= mapM B.packCString
  initVulkan glfwReqExts (`withWindowSurface` win)

runFrames
  :: GLFW.Window
  -> VulkanResources
  -> (Swapchain -> Acquire renderer) -- ^ Acquire renderer
  -> (renderer -> FrameContext -> IO ()) -- ^ Execute a frame
  -> IO ()
runFrames win vulkanResources acquireRenderer f = do
  let imguiAcquire swap =
        (,) <$> initDearImGuiForGLFW win vulkanResources swap
            <*> acquireRenderer swap
      imguiRender (imguiRes, userRes) frameContext = do
        renderDearImGui imguiRes frameContext glfwNewFrame do
          f userRes frameContext

  -- TODO: Option to turn off dear-imgui?
  -- (exeFrame, cleanup) <- buildFrameFunction glfwReqExts (uncurry Size <$> GLFW.getFramebufferSize win) (`withWindowSurface` win) acquireUserResources f
  (exeFrame, cleanup) <- buildFrameFunction vulkanResources (uncurry Size <$> GLFW.getFramebufferSize win) imguiAcquire imguiRender

  let
    glfwRunFrame = do
      GLFW.pollEvents
      exeFrame
      runCleanup vulkanResources
      GLFW.windowShouldClose win

  fix \rec -> liftIO glfwRunFrame >>= \case
    False -> rec
    True -> pure ()
  liftIO cleanup
