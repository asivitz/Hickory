module Hickory.FRP.Historical
  ( historicalWithEvents
  )
  where

import Reactive.Banana (Behavior, Event, MonadMoment, mapAccum)
import qualified Data.Sequence as S

-- Evolve a state according to a step function, while collecting events
historicalWithEvents
  :: MonadMoment m
  => a -- Initial state
  -> Event (Int, a -> (a,[b])) -- The indes to act on and a step function producing a new state and some 'events'
  -> m ( Behavior (S.Seq a)
       , Event [b]      -- New events
       )
historicalWithEvents initial eStep = do
  (evs, history) <- mapAccum (S.singleton initial) $
    append <$> eStep
  pure (history, evs)

maxLen :: Int
maxLen = 500

append :: (Int, a1 -> (a1, a2)) -> S.Seq a1 -> (a2, S.Seq a1)
append (idx, stepF) history =
  let history' = if not (null history) then S.take maxLen (S.drop idx history) else history
  in  case S.viewl history' of
        S.EmptyL -> error "Shouldn't happen"
        x S.:< _ -> let (acc, evs) = stepF x in (evs, acc S.<| history')
