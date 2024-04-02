{-# LANGUAGE DuplicateRecordFields #-}

module Platforms.GLFW.GameLoop where

import qualified Graphics.UI.GLFW as GLFW
import qualified Hickory.Vulkan.Types as H
import Platforms.GLFW.Bridge (glfwFrameBuilder, getWindowSizeRef)
import Data.Time (NominalDiffTime)
import qualified Platforms.GLFW.Vulkan as GLFWV
import Hickory.Vulkan.Renderer.Types (Scene)
import Acquire.Acquire (Acquire)
import Hickory.GameLoop (gameLoop)
import Data.IORef (readIORef)

glfwGameLoop
  :: GLFW.Window
  -> H.VulkanResources
  -> (H.Swapchain -> Acquire swapchainResources)
  -> NominalDiffTime
  -> ((Scene swapchainResources -> IO ()) -> IO (Scene swapchainResources))
  -> IO ()
glfwGameLoop win vulkanResources acquireSwapchainResources physicsTimeStep initialScene = do
  scrSizeRef <- getWindowSizeRef win
  frameBuilder <- glfwFrameBuilder win
  gameLoop (readIORef scrSizeRef) acquireSwapchainResources physicsTimeStep frameBuilder (GLFWV.runFrames win vulkanResources) initialScene


      -- focused <- GLFW.getWindowFocused win
      -- -- don't consume CPU when the window isn't focused
      -- unless focused (threadDelay 100000)

nullRenderF _ _ _ = pure () -- TODO: If we don't draw on the first app frame we get errors :(
{-
nullRenderF :: MonadIO m => Size Scalar -> (H.Renderer, H.FrameContext) -> m ()
nullRenderF _ (renderer, frameContext) = H.renderToRenderer frameContext renderer renderSettings (pure ()) (pure ())
  where
  renderSettings = H.RenderSettings
    { worldSettings = H.worldSettingsDefaults
    , overlayGlobals = OverlayGlobals
      { viewMat = identity
      , projMat = identity
      , viewProjMat = identity
      }
    , postSettings = H.postDefaults
    , clearColor = V4 0 0 0 1
    , highlightObjs = []
    }
-}
