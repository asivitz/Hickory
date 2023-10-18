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
import Hickory.Vulkan.Forward.Types (RenderSettings(..), OverlayGlobals (..))
import Linear (identity, V4 (..))
import qualified Hickory.Vulkan.Forward.Renderer as H
import qualified Platforms.GLFW.Vulkan as GLFWV
import Control.Concurrent (threadDelay)

pureGameScene :: Interpolatable model => model -> (InputFrame -> model -> (model, b)) -> (model -> RenderFunction) -> (Scene -> IO ()) -> IO (InputFrame -> IO (Scalar -> RenderFunction))
pureGameScene initialModel stepFunction renderF _sceneChange = do
  stateRef    :: IORef (S.Seq model) <- newIORef $ S.singleton initialModel
  stateIdxRef :: IORef Int <- newIORef 0
  pure \inputFrame -> do
    lastState <- readIORef stateRef <&> fromMaybe (error "No game states available") . S.lookup 0
    newState <- fmap getCompact . compact . fst $ stepFunction inputFrame lastState
    modifyIORef' stateRef (\s -> S.take 500 $ newState S.<| s)
    let interpolated frac = glerp frac lastState newState
    pure $ renderF . interpolated

  -- mdl <- ((,) <$> readIORef stateRef <*> readIORef stateIdxRef) <&> \(gameSeq, idx) ->
  --   let idx' = min idx (S.length gameSeq - 2)
  --   in case (,) <$> S.lookup idx' gameSeq <*> S.lookup (idx'+1) gameSeq of
  --       Just (to, from) -> glerp (realToFrac $ curInputFrame.delta / physicsTimeStep) from to
  --       Nothing -> fromMaybe (error "No game states available") $ S.lookup 0 gameSeq

type RenderFunction = Size Scalar -> (H.Renderer, H.FrameContext) -> IO ()
type Scene = InputFrame -> IO (Scalar -> RenderFunction)

gameLoop
  :: GLFW.Window
  -> H.VulkanResources
  -> NominalDiffTime
  -> ((Scene -> IO ()) -> IO Scene)
  -> IO ()
gameLoop win vulkanResources physicsTimeStep initialScene = do
  frameBuilder <- glfwFrameBuilder win
  scrSizeRef <- getWindowSizeRef win
  inputRef   :: IORef InputFrame <- newIORef mempty
  renderFRef :: IORef (Scalar -> Size Scalar -> (H.Renderer, H.FrameContext) -> IO ()) <- newIORef (const nullRenderF)
  sceneRef   :: IORef Scene <- newIORef undefined
  is <- initialScene (writeIORef sceneRef)
  writeIORef sceneRef is

  Ki.scoped \scope -> do
    _thr <- Ki.fork scope do
      forever do
        batched <- atomicModifyIORef' inputRef \cur ->
          let timeRemaining = physicsTimeStep - cur.delta
          in if timeRemaining > 0
              then (cur, Left timeRemaining)
              else (mempty { heldKeys = cur.heldKeys }, Right cur { delta = physicsTimeStep })
        case batched of
          Left timeRemaining -> threadDelay (ceiling @Double $ realToFrac timeRemaining * 1000000)
          Right inputFrame -> do
            scene <- readIORef sceneRef
            scene inputFrame >>= writeIORef renderFRef

    GLFWV.runFrames win vulkanResources (H.withRenderer vulkanResources) \renderer frameContext -> do
      inputFrame <- frameBuilder
      modifyIORef' inputRef (inputFrame<>)
      curInputFrame <- readIORef inputRef

      scrSize <- readIORef scrSizeRef
      readIORef renderFRef >>= \f -> f (realToFrac $ curInputFrame.delta / physicsTimeStep) (realToFrac <$> scrSize) (renderer, frameContext)

      -- focused <- GLFW.getWindowFocused win
      -- -- don't consume CPU when the window isn't focused
      -- unless focused (threadDelay 100000)

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
