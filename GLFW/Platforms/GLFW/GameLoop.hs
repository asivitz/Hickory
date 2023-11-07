{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Platforms.GLFW.GameLoop where

import Hickory.Math (Interpolatable (..), Scalar)
import Hickory.Input (InputFrame(..))
import Data.IORef (IORef, newIORef, readIORef, modifyIORef', writeIORef, atomicModifyIORef')
import qualified Graphics.UI.GLFW as GLFW
import qualified Hickory.Vulkan.Types as H
import Hickory.Types (Size)
import qualified Hickory.Vulkan.Forward.Types as H
import qualified Data.Sequence as S
import GHC.Compact (getCompact, compact)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Platforms.GLFW.Bridge (glfwFrameBuilder, getWindowSizeRef)
import qualified Ki
import Control.Monad (forever)
import Data.Time (NominalDiffTime)
import Control.Monad.IO.Class (MonadIO)
import Hickory.Vulkan.Forward.Types (RenderSettings(..), OverlayGlobals (..), RenderFunction)
import Linear (identity, V4 (..))
import qualified Hickory.Vulkan.Forward.Renderer as H
import qualified Platforms.GLFW.Vulkan as GLFWV
import Control.Concurrent (threadDelay)
import Hickory.Vulkan.Forward.Types (Scene)
import Acquire.Acquire (Acquire)

gameLoop
  :: GLFW.Window
  -> H.VulkanResources
  -> (H.Swapchain -> Acquire swapchainResources)
  -> NominalDiffTime
  -> ((Scene swapchainResources -> IO ()) -> IO (Scene swapchainResources))
  -> IO ()
gameLoop win vulkanResources acquireSwapchainResources physicsTimeStep initialScene = do
  frameBuilder <- glfwFrameBuilder win
  scrSizeRef <- getWindowSizeRef win
  inputRef   :: IORef InputFrame <- newIORef mempty
  sceneRef   :: IORef (Scene swapchainResources) <- newIORef undefined
  is <- initialScene (writeIORef sceneRef)
  writeIORef sceneRef is
  renderFRef :: IORef (Scalar -> InputFrame -> RenderFunction swapchainResources) <- newIORef =<< is mempty

  Ki.scoped \scope -> do
    _thr <- Ki.fork scope do
      forever do
        batched <- atomicModifyIORef' inputRef \cur ->
          let timeRemaining = physicsTimeStep - cur.delta
          in if timeRemaining > 0
              then (cur, Left timeRemaining)
              else (mempty { heldKeys = cur.heldKeys, delta = cur.delta - physicsTimeStep }, Right cur { delta = physicsTimeStep })
        case batched of
          Left timeRemaining -> threadDelay (ceiling @Double $ realToFrac timeRemaining * 1000000)
          Right inputFrame -> do
            scene <- readIORef sceneRef
            scene inputFrame >>= writeIORef renderFRef

    GLFWV.runFrames win vulkanResources acquireSwapchainResources \swapchainResources frameContext -> do
      inputFrame <- frameBuilder
      modifyIORef' inputRef (inputFrame<>)
      curInputFrame <- readIORef inputRef

      scrSize <- readIORef scrSizeRef
      readIORef renderFRef >>= \f -> f (realToFrac $ curInputFrame.delta / physicsTimeStep) inputFrame scrSize (swapchainResources, frameContext)

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
