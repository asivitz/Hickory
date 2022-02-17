module Hickory.FRP.Historical
  ( historical
  , historicalWithEvents
  , historicalInterpolateWithEvents
  )
  where

import Hickory.Math (Scalar)
import Hickory.Math.Interpolate (Interpolatable(..))
import Hickory.FRP.Combinators (unionFirst)
import Reactive.Banana (Behavior, Event, MonadMoment, mapAccum)
import qualified Data.Sequence as S

historical :: MonadMoment m => a -> Event (a -> a) -> Event Int -> m (Behavior a)
historical initial eStep eChangeIndex =
  fst <$> historicalWithEvents initial (((,[]) .) <$> eStep) eChangeIndex

historicalWithEvents :: MonadMoment m => a -> Event (a -> (a,[b])) -> Event Int -> m (Behavior a, Event [b])
historicalWithEvents initial eStep eChangeIndex = do
  (evs, history) <- mapAccum (S.singleton initial, 0) $
    unionFirst [ append <$> eStep
               , changeIdx <$> eChangeIndex
               ]
  pure (uncurry S.index <$> history, evs)

historicalInterpolateWithEvents :: (MonadMoment m, Interpolatable a) => a -> Behavior Scalar -> Event (a -> (a,[b])) -> Event Int -> m (Behavior a, Event [b])
historicalInterpolateWithEvents initial interp eStep eChangeIndex = do
  (evs, history) <- mapAccum (S.singleton initial, 0) $
    unionFirst [ append <$> eStep
               , changeIdx <$> eChangeIndex
               ]
  pure (assemble <$> interp <*> history, evs)
  where
  assemble frac (s,i) = let from = S.index s (min (i + 1) (S.length s - 1))
                            to = S.index s i
                        in glerp frac from to

maxLen :: Int
maxLen = 500

changeIdx :: Int -> (S.Seq a1, Int) -> ([a2], (S.Seq a1, Int))
changeIdx delta (s, i) = ([], (s, min (max 0 (i + delta)) (S.length s - 1)))

append :: Num b => (a1 -> (a1, a2)) -> (S.Seq a1, Int) -> (a2, (S.Seq a1, b))
append stepF (rest, i) =
  let rst  = if i > 0 then S.drop i rest else rest
      rst' = if not (null rst) then S.take maxLen rst else rst
  in  case S.viewl rst' of
        S.EmptyL -> error "Shouldn't happen"
        x S.:< _ -> let (acc, evs) = stepF x in (evs, (acc S.<| rst', 0))
