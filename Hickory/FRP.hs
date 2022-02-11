{-# LANGUAGE RecordWildCards #-}

module Hickory.FRP where

import Control.Applicative (liftA2)
import Control.Lens (_1, _2, _3, view)
import Control.Monad.Random.Lazy (runRand, Rand, liftIO)
import Data.Hashable (Hashable)
import Data.IORef (IORef, readIORef)
import Hickory.Input (TouchEvent(..), RawInput(..), Key)
import Hickory.Math.Vector (Scalar)
import Hickory.Utils.Utils (makeFPSTicker)
import Hickory.Types (Size)
import Reactive.Banana (Behavior, Event, MonadMoment, (<@>), mapAccum, unions, accumB, filterE, filterJust, First(..), getFirst, stepper)
import Reactive.Banana.Frameworks (fromAddHandler, newAddHandler, MomentIO, AddHandler, Handler, fromPoll)
import System.Random (StdGen, newStdGen)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Sequence as S
import Linear (V2(..))
import Data.Time (NominalDiffTime)

type HandlerPair a = (AddHandler a, Handler a)

mkEvent :: (AddHandler a, b) -> MomentIO (Event a)
mkEvent = fromAddHandler . fst

fire :: (AddHandler a, b) -> b
fire = snd

type Point = (V2 Scalar, Int) -- Location, Ident
type PointUp = (Scalar, V2 Scalar, Int) -- Duration, Location, Ident

touchHandlers :: IO (HandlerPair a, HandlerPair b, HandlerPair c)
touchHandlers =
  (,,) <$> newAddHandler <*> newAddHandler <*> newAddHandler

mkTouchEvents
  :: ( HandlerPair [Point]
     , HandlerPair [PointUp]
     , HandlerPair [Point]
     )
  -> MomentIO (Event [Point], Event [PointUp], Event [Point])
mkTouchEvents (pointDownPair, pointUpPair, pointLocPair) = do
  ePointDown <- mkEvent pointDownPair
  ePointUp   <- mkEvent pointUpPair
  ePointLoc  <- mkEvent pointLocPair

  pure (ePointDown, ePointUp, ePointLoc)


concatTouchEvents :: CoreEvents -> Event [TouchEvent]
concatTouchEvents CoreEvents {..} = mconcat
  [ map (Down . fst)     <$> eTouchesDown
  , map (Up   . view _2) <$> eTouchesUp
  , map (Loc  . fst)     <$> eTouchesLoc
  ]

keyHandlers :: IO (HandlerPair a, HandlerPair b, HandlerPair c)
keyHandlers = (,,) <$> newAddHandler <*> newAddHandler <*> newAddHandler

mkKeyEvents :: (Ord b1, Fractional b1, Hashable k, Eq k) => (HandlerPair k, HandlerPair (HashMap.HashMap k b1), HandlerPair k) -> MomentIO (Event k, k -> Event k, Event k)
mkKeyEvents (keyPair, keysHeldPair, keyUpPair) = do
  eKey    <- mkEvent keyPair
  eKeysHeld <- mkEvent keysHeldPair
  eKeyUp  <- mkEvent keyUpPair
  let -- keyDown k = filterE (==k) eKey
      keyDownOrHeld k = unionFirst [ filterE (==k) eKey
                                   , k <$ (filterE (>0.2) . filterJust $ HashMap.lookup k <$> eKeysHeld)
                                   ]
      -- keyUp k = filterE (==k) eKeyUp
  pure (eKey, keyDownOrHeld, eKeyUp)

data CoreEventGenerators = CoreEventGenerators
  { renderEvent :: HandlerPair Scalar
  , touchEvents :: ( HandlerPair [(V2 Scalar, Int)]
                   , HandlerPair [(Double, V2 Scalar, Int)]
                   , HandlerPair [(V2 Scalar, Int)])
  , keyEvents   :: (HandlerPair Key, HandlerPair (HashMap.HashMap Key Double), HandlerPair Key)
  , timeEvents  :: HandlerPair NominalDiffTime
  , windowSize  :: IORef (Size Int)
  }

data CoreEvents = CoreEvents
  { eRender       :: Event Scalar
  , eTime         :: Event NominalDiffTime
  , keyDown       :: Event Key
  , keyDownOrHeld :: Key -> Event Key
  , keyUp         :: Event Key
  , eTouchesDown  :: Event [Point]
  , eTouchesLoc   :: Event [Point]
  , eTouchesUp    :: Event [PointUp]
  , scrSizeB      :: Behavior (Size Int)
  , fpsB          :: Behavior Scalar
  , currentTimeB  :: Behavior NominalDiffTime
  , eNewTime      :: Event NominalDiffTime
  }

coreEventGenerators :: IO [RawInput] -> IO NominalDiffTime -> IORef (Size Int) -> IO (IO (), CoreEventGenerators)
coreEventGenerators inputPoller timePoller wSizeRef = do

  touchPairs <- touchHandlers
  keyPairs   <- keyHandlers
  timePair   <- newAddHandler
  renderPair <- newAddHandler

  fpsTicker  <- makeFPSTicker

  let processor = do
        fps <- fpsTicker
        fire renderPair fps

        (inputPoller >>=) . mapM_ $ \case
          InputKeyDown k -> fire (view _1 keyPairs) k
          InputKeysHeld keymap -> fire (view _2 keyPairs) keymap
          InputKeyUp k _ -> fire (view _3 keyPairs) k
          InputTouchesDown touches -> fire (view _1 touchPairs) touches
          InputTouchesUp   touches -> fire (view _2 touchPairs) touches
          InputTouchesLoc  touches -> fire (view _3 touchPairs) touches

        timePoller >>= fire timePair

  pure (processor, CoreEventGenerators renderPair touchPairs keyPairs timePair wSizeRef)

mkCoreEvents :: CoreEventGenerators -> MomentIO CoreEvents
mkCoreEvents coreEvGens = do
  (eTouchesDown, eTouchesUp, eTouchesLoc) <- mkTouchEvents . touchEvents $ coreEvGens
  (keyDown, keyDownOrHeld, keyUp)         <- mkKeyEvents   . keyEvents $ coreEvGens
  eTime   <- mkEvent . timeEvents $ coreEvGens
  eRender <- mkEvent . renderEvent $ coreEvGens

  scrSizeB <- fromPoll . readIORef . windowSize $ coreEvGens
  fpsB     <- stepper 0 eRender

  (eNewTime, currentTimeB) <- mapAccum 0 $ (\timeDelt x -> (timeDelt + x, timeDelt + x)) <$> eTime

  pure $ CoreEvents { .. }

-- Utils

-- Helpers

accumEvents :: MonadMoment m => a -> Behavior (a -> b -> a) -> Event [b] -> m (Behavior a)
accumEvents b bf e = accumB b (fol <$> bf <@> e)
  where fol f bs a = foldr (flip f) a bs

randomEvent :: Event (Rand StdGen a) -> MomentIO (Event a)
randomEvent b = do
  initialStdGen <- liftIO newStdGen
  fst <$> mapAccum initialStdGen (runRand <$> b)

foldE :: (a -> b -> a) -> a -> [b] -> a
foldE x = foldr (flip x)

unionFirst :: [Event a] -> Event a
unionFirst = fmap getFirst . mconcat . fmap (fmap First)

noEvs :: (a -> b -> (a, [c])) -> a -> b -> a
noEvs f a b = fst $ f a b

foldEventEmitter :: (a -> b -> (a, [c])) -> a -> [b] -> (a, [c])
foldEventEmitter f a = foldr (flip $ evFolder f) (a, [])

evFolder :: (a -> b -> (a, [c])) -> (a, [c]) -> b -> (a, [c])
evFolder f (a, cs) b = (<> cs) <$> f a b

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
  pure (uncurry S.index <$> history, evs)
 where
  maxLen = 500
  changeIdx delta (s, i) = ([], (s, min (max 0 (i + delta)) (S.length s - 1)))
  append stepF (rest, i) =
    let rst  = if i > 0 then S.drop i rest else rest
        rst' = if not (null rst) then S.take maxLen rst else rst
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
