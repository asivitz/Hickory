module Hickory.FRP.Historical
  ( historical
  , historicalWithEvents
  )
  where

import Hickory.FRP.Combinators (unionFirst)
import Reactive.Banana (Behavior, Event, MonadMoment, mapAccum, never)
import qualified Data.Sequence as S

historical :: MonadMoment m => a -> Event (a -> a) -> m (Behavior a)
historical initial eStep = do
  (history, _) <- historicalWithEvents initial (((,[]) .) <$> eStep) never
  pure $
    (\s -> case S.viewl s of
      S.EmptyL -> error "Shouldn't happen"
      x S.:< _ -> x
    ) <$> history

-- Evolve a state according to a step function, while collecting events
historicalWithEvents
  :: MonadMoment m
  => a -- Initial state
  -> Event (a -> (a,[b])) -- Step function producing a new state and some 'events'
  -> Event (S.Seq a -> S.Seq a) -- Modify the history
  -> m ( Behavior (S.Seq a)
       , Event [b]      -- New events
       )
historicalWithEvents initial eStep eModify = do
  (evs, history) <- mapAccum (S.singleton initial) $
    unionFirst [ append <$> eStep
               , (\f s -> ([], f s)) <$> eModify
               ]
  pure (history, evs)

maxLen :: Int
maxLen = 500

append :: (a1 -> (a1, a2)) -> S.Seq a1 -> (a2, S.Seq a1)
append stepF history =
  let history' = if not (null history) then S.take maxLen history else history
  in  case S.viewl history' of
        S.EmptyL -> error "Shouldn't happen"
        x S.:< _ -> let (acc, evs) = stepF x in (evs, acc S.<| history')
