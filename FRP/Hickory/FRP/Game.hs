module Hickory.FRP.Game where

import Hickory.Math.Interpolate (Interpolatable)
import Hickory.FRP.Combinators (unionFirst)
import Data.Time (NominalDiffTime)
import qualified Reactive.Banana as B
import qualified Reactive.Banana.Frameworks as B
import Reactive.Banana ((<@>))
import Data.Tuple (swap)

-- Queue up events and release in a batch
-- For example, to collect a frame's worth of input events and process at
-- the start of the next frame.
batchEvents
  :: B.Event [acc]
  -> B.Event a
  -> B.MomentIO (B.Event (a, [acc]))
batchEvents accEv batchEv = do
  (ev, _be) <- B.mapAccum [] $ B.mergeWith
    (\acc accs -> (Nothing, acc ++ accs))
    (\a acc -> (Just (a, acc), []))
    (\acc a acc2 -> (Just (a, acc ++ acc2), []))
    accEv
    batchEv

  pure $ B.filterJust ev

chunkTime :: NominalDiffTime -> B.Event NominalDiffTime -> B.MomentIO (B.Event NominalDiffTime, B.Behavior Double)
chunkTime chunkSize e = do
  (ev,b) <- B.mapAccum 0 $ (\chunk acc -> if chunk + acc > chunkSize
                                           then (Just chunkSize, acc + chunk - chunkSize)
                                           else (Nothing, chunk + acc))
                         <$> e
  pure (B.filterJust ev, realToFrac . (/chunkSize) <$> b)

timeStep :: NominalDiffTime -> B.Event [input] -> B.Event NominalDiffTime -> B.MomentIO (B.Behavior Double, B.Event (NominalDiffTime, [input]))
timeStep physicsTimeStep controlComs eInGameTime = do
  (   eGameTime -- ticks with a new chunk of game time whenever enough has been accumulated
    , frameFraction -- the fraction of a chunk we have progressed
    ) <- chunkTime physicsTimeStep eInGameTime
  eFrameInput <- batchEvents controlComs eGameTime
  pure (frameFraction, eFrameInput)

loadOrStep :: B.Event state -> B.Event (state -> (state, [event])) -> B.Event (state -> (state, [event]))
loadOrStep eLoad eStep = unionFirst
  [ eStep
  , const <$> ((,[]) <$> eLoad)
  ]
