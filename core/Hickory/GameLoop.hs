{-# LANGUAGE OverloadedRecordDot #-}

module Hickory.GameLoop where

import Hickory.Math (Interpolatable (..), Scalar, clamp)
import qualified Data.Sequence as S
import Data.Maybe (fromMaybe)
import Data.Fixed (mod')
import Linear (nearZero)

import Hickory.Input (InputFrame(..), RawInput, inputFrameBuilder)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef', writeIORef)
import qualified Ki
import Data.Time (NominalDiffTime, getCurrentTime, UTCTime, diffUTCTime, addUTCTime)
import Control.Concurrent (threadDelay)
import Control.Monad.Extra (whileM, untilJustM)
import Data.Foldable (for_)

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

-- For more predictable timing, keep the fields of your game state strict
-- (Use the StrictData extension)
gameLoop
  :: (Interpolatable a)
  => NominalDiffTime
  -> IO [RawInput]
  -> IO (Maybe b)
  -> [a]
  -> (InputFrame -> IO a)
  -> (a -> IO ())
  -> IO b
gameLoop physicsTimeStep inputPoller termination initialStates physF renderF = do
  -- Triple buffer recorded game states
  initialFrameTime <- getCurrentTime
  statesRef :: IORef (UTCTime, Word, [a]) <- newIORef (initialFrameTime, 0, initialStates)

  builder <- inputFrameBuilder
  inputsRef <- newIORef []
  logicThreadAlive <- newIORef True

  Ki.scoped \scope -> do
    _thr <- Ki.fork scope do
      whileM do
        (lastFrameTime, curFrameNum, _) <- readIORef statesRef
        currentTime <- getCurrentTime
        let diff = diffUTCTime currentTime lastFrameTime
        if diff > physicsTimeStep
        then do
          input <- atomicModifyIORef' inputsRef ([],)
          inputFrame <- builder input physicsTimeStep
          !a <- physF $ inputFrame { frameNum = curFrameNum }
          atomicModifyIORef' statesRef \(lft, fn, ss) ->
            ((if diff > 1 then currentTime else addUTCTime physicsTimeStep lft, fn + 1, a : take 2 ss), ())
        else
          threadDelay 1000 -- 1 ms

        (readIORef logicThreadAlive)

    exitVal <- untilJustM do
      (lastFrameTime, _, ss) <- readIORef statesRef
      input <- inputPoller -- Need to poll input on main thread (on SDL at least)
      atomicModifyIORef' inputsRef \is -> (is ++ input, ())
      currentTime <- getCurrentTime
      let frac = realToFrac $ (diffUTCTime currentTime lastFrameTime) / physicsTimeStep
          interpState = case ss of
            -- Most of the time we're interpolating the 2 oldest states, so
            -- there's a slight lag
            [_latest, middle, oldest] | frac < 1 -> Just $ glerp frac oldest middle
            [latest, middle, _oldest]            -> Just $ glerp (clamp (frac - 1) 0 1) middle latest
            [latest, prev] -> Just $ glerp (clamp frac 0 1) prev latest
            [one] -> Just one
            _ -> Nothing

      for_ interpState renderF
      termination
    writeIORef logicThreadAlive False
    pure exitVal
