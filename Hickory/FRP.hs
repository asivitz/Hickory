module Hickory.FRP where

import Control.Lens (_1, _2, _3, view)
import qualified Data.HashMap.Strict as HashMap
import Hickory.Math.Vector (V4(..), V2(..), Scalar, V3(..), v2tov3)
import Data.Hashable (Hashable)
import Hickory.Input (TouchEvent(..))
import Reactive.Banana (Behavior, Event, MonadMoment, (<@>), mapAccum, unions, accumB, filterE, filterJust, First(..), getFirst)
import Reactive.Banana.Frameworks (fromAddHandler, newAddHandler, MomentIO, AddHandler, Handler)
import Control.Monad.Random.Lazy (runRand, Rand, liftIO)
import System.Random (StdGen, newStdGen)
import qualified Data.Sequence as S
import Control.Applicative (liftA2)


type HandlerPair a = (AddHandler a, Handler a)

mkEvent :: (AddHandler a, b) -> MomentIO (Event a)
mkEvent = fromAddHandler . fst

fire :: (AddHandler a, b) -> b
fire = snd

touchHandlers :: IO (HandlerPair a, HandlerPair b, HandlerPair c)
touchHandlers =
  (,,) <$> newAddHandler <*> newAddHandler <*> newAddHandler

touchEvents
  :: ( HandlerPair [(V2 Scalar, b)]
     , HandlerPair [(Double, V2 Scalar, Int)]
     , HandlerPair [(V2 Scalar, b3)]
     )
  -> MomentIO (Event [TouchEvent])
touchEvents (pointDownPair, pointUpPair, pointLocPair) = do
  ePointDown <- mkEvent pointDownPair
  ePointUp   <- mkEvent pointUpPair
  ePointLoc  <- mkEvent pointLocPair

  let eTouchEvs = mconcat
        [ map (Down . fst)     <$> ePointDown
        , map (Up   . view _2) <$> ePointUp
        , map (Loc  . fst)     <$> ePointLoc
        ]

  pure eTouchEvs

keyHandlers :: IO (HandlerPair a, HandlerPair b)
keyHandlers = (,) <$> newAddHandler <*> newAddHandler

keyEvents :: (Ord b1, Fractional b1, Hashable k, Eq k) => (HandlerPair k, HandlerPair (HashMap.HashMap k b1)) -> MomentIO (k -> Event k, k -> Event k)
keyEvents (keyPair, keysHeldPair) = do
  eKey    <- mkEvent keyPair
  eKeysHeld <- mkEvent keysHeldPair
  let keyDown k = filterE (==k) eKey
      keyDownOrHeld k = unionFirst [ keyDown k
                                   , k <$ (filterE (>0.2) . filterJust $ HashMap.lookup k <$> eKeysHeld)
                                   ]
  pure (keyDown, keyDownOrHeld)

-- Utils

-- Helpers

accumEvents :: MonadMoment m => a -> Behavior (a -> b -> a) -> Event [b] -> m (Behavior a)
accumEvents b bf e = accumB b (fol <$> bf <@> e)
  where fol f bs a = foldr (flip f) a bs

randomEvent :: Event (Rand StdGen a) -> MomentIO (Event a)
randomEvent b = do
  initialStdGen <- liftIO $ newStdGen
  fst <$> mapAccum initialStdGen (runRand <$> b)

foldE :: (a -> b -> a) -> a -> [b] -> a
foldE x = foldr (flip x)

unionFirst :: [Event a] -> Event a
unionFirst = fmap getFirst . mconcat . fmap (fmap First)

noEvs :: (a -> b -> (a, [c])) -> a -> b -> a
noEvs f a b = fst $ f a b

foldEventEmitter :: (a -> b -> (a, [c])) -> a -> [b] -> (a, [c])
foldEventEmitter f a bs = foldr (flip $ evFolder f) (a, []) bs

evFolder :: (a -> b -> (a, [c])) -> (a, [c]) -> b -> (a, [c])
evFolder f (a, cs) b = fmap (<> cs) $ f a b

counter :: MonadMoment m => Event Int -> Event Int -> m (Behavior Int)
counter eDelta eReset = accumB 0 $ unions [ (+) <$> eDelta, const <$> eReset ]

historical :: MonadMoment m => a -> Event (a -> a) -> Event Int -> m (Behavior a)
historical initial eStep eChangeIndex =
  fst <$> historicalWithEvents initial (((,[]) .) <$> eStep) eChangeIndex

historicalWithEvents :: MonadMoment m => a -> Event (a -> (a,[b])) -> Event Int -> m (Behavior a, Event [b])
historicalWithEvents initial eStep eChangeIndex = do
  (evs, history) <- mapAccum (S.singleton initial, 0) $
    unionFirst [ append <$> eStep
               , changeIdx <$> eChangeIndex
               ]
  pure $ ((\(s, i) -> S.index s i) <$> history, evs)
 where
  maxLen = 500
  changeIdx delta (s, i) = ([], (s, min (max 0 (i + delta)) (S.length s - 1)))
  append stepF (rest, i) =
    let rst  = if i > 0 then S.drop i rest else rest
        rst' = if length rst > 0 then S.take maxLen rst else rst
    in  case S.viewl rst' of
          S.EmptyL -> error "SHouldn't happen"
          x S.:< _ -> let (acc, evs) = stepF x in (evs, (acc S.<| rst', 0))

-- useful for [Behavior (Resources -> RenderTree)] -> Behavior (Resources -> [RenderTree])
combineRenderFuncs :: Applicative f => [f (a -> b)] -> f (a -> [b])
combineRenderFuncs = foldr (liftA2 go) (pure $ const [])
  where go :: (a -> b) -> (a -> [b]) -> a -> [b]
        go f fs r = f r : fs r

-- Unused

splitPair :: Event (a, b) -> (Event a, Event b)
splitPair e = (fst <$> e, snd <$> e)

foldEvents :: Behavior b -> (b -> a -> b) -> Event [a] -> Event b
foldEvents b h e = fmap (foldE h) b <@> e

foldEvents' :: Behavior (a -> b -> a) -> Behavior a -> Event [b] -> Event a
foldEvents' bf b e = foldE <$> bf <*> b <@> e
