module Hickory.ImGUI.SDL where

import qualified DearImGui as ImGui
import Hickory.ImGUI.ImGUI (renderDearImGui, initDearImGui)
import DearImGui.SDL3.Vulkan (sdl3InitForVulkan)
import DearImGui.SDL3 (sdl3Shutdown, sdl3NewFrame, pollEventsWithImGui)

import Hickory.Types
import Linear (V2(..))
import qualified SDL
import Acquire (Acquire)
import Hickory.Vulkan.Types (VulkanResources, Swapchain, FrameContext, runCleanup)
import qualified SDL.Vulkan as SDL
import Hickory.Vulkan.Utils (buildFrameFunction)
import Control.Monad (void)
import Platforms.SDL3 (SDLHandles, sdlFrameBuilder')


sdlFrameBuilder :: SDL.SDLWindow -> IO SDLHandles
sdlFrameBuilder win = sdlFrameBuilder' win pollEventsWithImGui ImGui.wantCaptureMouse ImGui.wantCaptureKeyboard

runFrames
  :: SDL.SDLWindow
  -> VulkanResources
  -> (Swapchain -> Acquire renderer) -- ^ Acquire renderer
  -> Acquire ((renderer -> FrameContext -> IO ()) -> IO ())
runFrames win vulkanResources acquireRenderer = do
  let imguiAcquire swap =
        (,) <$> initDearImGui (void $ sdl3InitForVulkan win) sdl3Shutdown vulkanResources swap
            <*> acquireRenderer swap
      imguiRender f (imguiRes, userRes) frameContext = do
        renderDearImGui imguiRes frameContext sdl3NewFrame do
          f userRes frameContext

  let queryPixelSize = do
        SDL.sdlGetWindowSizeInPixels win >>= \case
          Nothing -> error "Couldn't get window size"
          Just (x,y) -> pure $ Size x y

  -- TODO: Option to turn off dear-imgui?
  exeFrame <- buildFrameFunction vulkanResources queryPixelSize imguiAcquire

  pure \f -> do
    exeFrame (imguiRender f)
    runCleanup vulkanResources
