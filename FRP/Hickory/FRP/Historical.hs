module Hickory.FRP.Historical
  ( historical
  , historicalWithEvents
  )
  where

import Hickory.FRP.Combinators (unionFirst)
import Reactive.Banana (Behavior, Event, MonadMoment, mapAccum)
import qualified Data.Sequence as S

historical :: MonadMoment m => a -> Event (a -> a) -> Event Int -> m (Behavior (a,a))
historical initial eStep eChangeIndex =
  fst <$> historicalWithEvents initial (((,[]) .) <$> eStep) eChangeIndex

-- Evolve a state according to a step function, while collecting events
historicalWithEvents
  :: MonadMoment m
  => a -- Initial state
  -> Event (a -> (a,[b])) -- Step function producing a new state and some 'events'
  -> Event Int -- Change current state by relative index (e.g. -1 means go back 1 frame)
  -> m ( Behavior (a,a) -- Previous and current states
       , Event [b]      -- New events
       )
historicalWithEvents initial eStep eChangeIndex = do
  (evs, history) <- mapAccum (S.singleton initial, 0) $
    unionFirst [ append <$> eStep
               , changeIdx <$> eChangeIndex
               ]
  pure (assemble <$> history, evs)
  where
  assemble (s,i) = let from = S.index s (min (i + 1) (S.length s - 1))
                       to = S.index s i
                   in (from, to)

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
