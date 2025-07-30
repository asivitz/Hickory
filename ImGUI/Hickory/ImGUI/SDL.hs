module Hickory.ImGUI.SDL where

import qualified DearImGui as ImGui
import Hickory.ImGUI.ImGUI (renderDearImGui, initDearImGui)
import DearImGui.SDL.Vulkan (sdl2InitForVulkan)
import DearImGui.SDL (sdl2Shutdown, sdl2NewFrame, pollEventsWithImGui)

sdlFrameBuilder :: IO SDLHandles
sdlFrameBuilder = sdlFrameBuilder' pollEventsWithImGui ImGui.wantCaptureMouse ImGui.wantCaptureKeyboard

runFrames
  :: SDL.Window
  -> VulkanResources
  -> (Swapchain -> Acquire renderer) -- ^ Acquire renderer
  -> Acquire ((renderer -> FrameContext -> IO ()) -> IO ())
runFrames win vulkanResources acquireRenderer = do
  let imguiAcquire swap =
        (,) <$> initDearImGui (void $ sdl2InitForVulkan win) sdl2Shutdown vulkanResources swap
            <*> acquireRenderer swap
      imguiRender f (imguiRes, userRes) frameContext = do
        renderDearImGui imguiRes frameContext sdl2NewFrame do
          f userRes frameContext

  -- TODO: Option to turn off dear-imgui?
  exeFrame <- buildFrameFunction vulkanResources ((\(V2 x y) -> Size x y) . fmap fromIntegral <$> SDL.vkGetDrawableSize win) imguiAcquire

  pure \f -> do
    exeFrame (imguiRender f)
    runCleanup vulkanResources
