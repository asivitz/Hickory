module Hickory.GameLoop where

import Hickory.Math (Interpolatable (..), Scalar)
import qualified Data.Sequence as S
import Data.Maybe (fromMaybe)
import Data.Fixed (mod')
import Linear (nearZero)

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

