module Hickory.FRP.Combinators where

import Control.Monad.Random.Lazy (Rand, liftIO, runRand)
import Reactive.Banana (Behavior, Event, First (..), MonadMoment, accumB, getFirst, mapAccum, unions, (<@>))
import Reactive.Banana.Frameworks (MomentIO)
import System.Random (StdGen, newStdGen)

unionFirst :: [Event a] -> Event a
unionFirst = fmap getFirst . mconcat . fmap (fmap First)

counter :: MonadMoment m => Event Int -> Event Int -> m (Behavior Int)
counter eDelta eReset = accumB 0 $ unions [ (+) <$> eDelta, const <$> eReset ]

randomEvent :: Event (Rand StdGen a) -> MomentIO (Event a)
randomEvent b = do
  initialStdGen <- liftIO newStdGen
  fst <$> mapAccum initialStdGen (runRand <$> b)

accumEvents :: MonadMoment m => a -> Behavior (a -> b -> a) -> Event [b] -> m (Behavior a)
accumEvents b bf e = accumB b (fol <$> bf <@> e)
  where fol f bs a = foldr (flip f) a bs
