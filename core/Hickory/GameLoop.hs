{-# LANGUAGE OverloadedRecordDot #-}

module Hickory.GameLoop where

import Hickory.Math (Interpolatable (..), Scalar)
import qualified Data.Sequence as S
import Data.Maybe (fromMaybe)
import Data.Fixed (mod')
import Linear (nearZero)

import Hickory.Input (InputFrame(..))
import Data.IORef (IORef, newIORef, readIORef, modifyIORef', writeIORef, atomicModifyIORef')
import qualified Hickory.Vulkan.Types as H
import Hickory.Types (Size)
import qualified Ki
import Control.Monad (forever)
import Data.Time (NominalDiffTime)
import Hickory.Vulkan.Renderer.Types ( RenderFunction, Scene )
import Control.Concurrent (threadDelay)
import Acquire (Acquire)

newGameStateStack :: a -> S.Seq a
newGameStateStack = S.singleton

stepGameState :: (a -> (a, b)) -> S.Seq a -> (S.Seq a, b)
stepGameState f s = let (model', evs) = f lastState in (S.take 500 $ model' S.<| s, evs)
  where
  lastState = fromMaybe (error "No game states available") . S.lookup 0 $ s

queryGameState :: Interpolatable i => S.Seq i -> Scalar -> i
queryGameState s (max 0 . min (realToFrac $ S.length s - 1) -> idx) =
  case (from, to) of
    _ | nearZero idx  -> fromMaybe (error "No game states available") $ S.lookup 0 s
    _ | nearZero frac -> fromMaybe (error "No game states available") $ from
    (Just fr, Just t) -> if frac > 0 then glerp (1 - frac) fr t else t
    _ -> fromMaybe (error "No game states available") $ S.lookup 0 s
  where
  frac = idx `mod'` 1
  to   = S.lookup (floor idx) s
  from = S.lookup (ceiling idx) s

{-
pureGameScene :: forall model evs. Interpolatable model => model -> (InputFrame -> model -> (model, evs)) -> IO (InputFrame -> IO (model, model))
pureGameScene initialModel stepFunction = do
  stateRef    :: IORef (S.Seq model) <- newIORef $ S.singleton initialModel
  stateIdxRef :: IORef Int <- newIORef 0
  pure \inputFrame -> do
    lastState <- readIORef stateRef <&> fromMaybe (error "No game states available") . S.lookup 0
    newState <- pure . fst $ stepFunction inputFrame lastState
    modifyIORef' stateRef (\s -> S.take 500 $ newState S.<| s)
    pure (lastState, newState)
    -}

  -- mdl <- ((,) <$> readIORef stateRef <*> readIORef stateIdxRef) <&> \(gameSeq, idx) ->
  --   let idx' = min idx (S.length gameSeq - 2)
  --   in case (,) <$> S.lookup idx' gameSeq <*> S.lookup (idx'+1) gameSeq of
  --       Just (to, from) -> glerp (realToFrac $ curInputFrame.delta / physicsTimeStep) from to
  --       Nothing -> fromMaybe (error "No game states available") $ S.lookup 0 gameSeq

gameLoop
  :: IO (Size Int)
  -> (H.Swapchain -> Acquire swapchainResources)
  -> NominalDiffTime
  -> IO InputFrame
  -> ((H.Swapchain -> Acquire swapchainResources) -> (swapchainResources -> H.FrameContext -> IO ()) -> IO ())
  -> ((Scene swapchainResources -> IO ()) -> IO (Scene swapchainResources))
  -> IO ()
gameLoop readScrSize acquireSwapchainResources physicsTimeStep frameBuilder runFrames initialScene = do
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
              else (mempty { heldKeys = cur.heldKeys, delta = cur.delta - physicsTimeStep, frameNum = cur.frameNum + 1 }, Right cur { delta = physicsTimeStep })
        case batched of
          Left timeRemaining -> threadDelay (ceiling @Double $ realToFrac timeRemaining * 1000000)
          Right inputFrame -> do
            scene <- readIORef sceneRef
            scene inputFrame >>= writeIORef renderFRef

    runFrames acquireSwapchainResources \swapchainResources frameContext -> do
      inputFrame <- frameBuilder
      modifyIORef' inputRef (inputFrame<>)
      curInputFrame <- readIORef inputRef

      scrSize <- readScrSize
      readIORef renderFRef >>= \f -> f (realToFrac $ curInputFrame.delta / physicsTimeStep) inputFrame { frameNum = curInputFrame.frameNum } scrSize (swapchainResources, frameContext)
