module Hickory.FRP where

import Control.Applicative (liftA2)
import Control.Lens (_1, _2, _3, view)
import Control.Monad.Random.Lazy (runRand, Rand, liftIO)
import Data.Hashable (Hashable)
import Data.IORef (IORef, readIORef)
import Hickory.Input (TouchEvent(..), RawInput(..), Key)
import Hickory.Math.Vector (V2(..), Scalar)
import Hickory.Utils.Utils (makeFPSTicker)
import Hickory.Types (Size)
import Reactive.Banana (Behavior, Event, MonadMoment, (<@>), mapAccum, unions, accumB, filterE, filterJust, First(..), getFirst, stepper)
import Reactive.Banana.Frameworks (fromAddHandler, newAddHandler, MomentIO, AddHandler, Handler, fromPoll)
import System.Random (StdGen, newStdGen)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Sequence as S
import qualified Hickory.Graphics.DrawUtils as DU
import qualified Reflex as R

type HandlerPair t a = (R.Event t a, a -> IO ())

-- mkEvent :: (AddHandler a, b) -> MomentIO (Event a)
-- mkEvent = fromAddHandler . fst

fire :: (R.Event t a, b) -> b
fire = snd

touchHandlers :: IO (HandlerPair t a, HandlerPair t b, HandlerPair t c)
touchHandlers =
  (,,) <$> R.newTriggerEvent <*> R.newTriggerEvent <*> R.newTriggerEvent

mkTouchEvents
  :: Monad m => ( HandlerPair t [(V2 Scalar, b)]
     , HandlerPair t [(Double, V2 Scalar, Int)]
     , HandlerPair t [(V2 Scalar, b3)]
     )
  -> m (R.Event t [TouchEvent])
mkTouchEvents (pointDownPair, pointUpPair, pointLocPair) = do
  let ePointDown = fst pointDownPair
  let ePointUp   = fst pointUpPair
  let ePointLoc  = fst pointLocPair

  let eTouchEvs = mconcat
        [ map (Down . fst)     <$> ePointDown
        , map (Up   . view _2) <$> ePointUp
        , map (Loc  . fst)     <$> ePointLoc
        ]

  pure eTouchEvs

keyHandlers :: IO (HandlerPair t a, HandlerPair t b, HandlerPair t c)
keyHandlers = (,,) <$> R.newTriggerEvent <*> R.newTriggerEvent <*> R.newTriggerEvent

mkKeyEvents :: (Ord b1, Fractional b1, Hashable k, Eq k) => Monad m => (HandlerPair t k, HandlerPair t (HashMap.HashMap k b1), HandlerPair t k) -> m (R.Event t k, k -> R.Event t k, R.Event t k)
mkKeyEvents (keyPair, keysHeldPair, keyUpPair) = do
  let eKey    = fst keyPair
  let eKeysHeld = fst keysHeldPair
  let eKeyUp  = fst keyUpPair
  let -- keyDown k = filterE (==k) eKey
      keyDownOrHeld k = unionFirst [ R.ffilter (==k) $ eKey
                                   , k <$ (R.ffilter (>0.2) . R.fmapMaybe (HashMap.lookup k) $ eKeysHeld)
                                   ]
      -- keyUp k = filterE (==k) eKeyUp
  pure (eKey, keyDownOrHeld, eKeyUp)

data CoreEventGenerators t = CoreEventGenerators
  { renderEvent :: HandlerPair t Scalar
  , touchEvents :: ( HandlerPair t [(V2 Scalar, Int)]
                   , HandlerPair t [(Double, V2 Scalar, Int)]
                   , HandlerPair t [(V2 Scalar, Int)])
  , keyEvents   :: (HandlerPair t Key, HandlerPair t (HashMap.HashMap Key Double), HandlerPair t Key)
  , stepEvents  :: HandlerPair t Scalar
  , windowSize  :: IORef (Size Int)
  }

data CoreEvents t = CoreEvents
  { eRender   :: R.Event t Scalar
  , eTouchEvs :: R.Event t [TouchEvent]
  , eTime     :: R.Event t Scalar
  , keyDown   :: R.Event t Key
  , keyDownOrHeld :: Key -> R.Event t Key
  , keyUp     :: R.Event t Key
  , scrSizeB  :: R.Behavior t (Size Int)
  , fpsB      :: R.Behavior t Scalar
  }

coreEventGenerators :: IO [RawInput] -> IORef (Size Int) -> IO (IO (), CoreEventGenerators t)
coreEventGenerators inputPoller wSizeRef = do

  touchPairs <- touchHandlers
  keyPairs   <- keyHandlers
  timePair   <- R.newTriggerEvent
  renderPair <- R.newTriggerEvent

  fpsTicker  <- makeFPSTicker

  let processor = do
        fps <- fpsTicker
        fire renderPair fps

        (inputPoller >>=) . mapM_ $ \case
          InputKeyDown k -> fire (view _1 keyPairs) k
          InputKeysHeld keymap -> fire (view _2 keyPairs) keymap
          InputKeyUp k _ -> fire (view _3 keyPairs) k
          Step d         -> fire timePair d
          InputTouchesDown touches -> fire (view _1 touchPairs) touches
          InputTouchesUp   touches -> fire (view _2 touchPairs) touches
          InputTouchesLoc  touches -> fire (view _3 touchPairs) touches
  pure (processor, CoreEventGenerators renderPair touchPairs keyPairs timePair wSizeRef)

mkCoreEvents :: Monad m => CoreEventGenerators t -> m (CoreEvents t)
mkCoreEvents coreEvGens = do
  eTouchEvs                <- mkTouchEvents . touchEvents $ coreEvGens
  let eTime                = fst . stepEvents $ coreEvGens
  (keyDown, keyDownOrHeld, keyUp) <- mkKeyEvents . keyEvents $ coreEvGens
  let eRender              = fst . renderEvent $ coreEvGens

  scrSizeB <- R.pull . readIORef . windowSize $ coreEvGens
  fpsB     <- stepper 0 eRender

  pure $ CoreEvents eRender eTouchEvs eTime keyDown keyDownOrHeld keyUp scrSizeB fpsB

-- Utils

-- Helpers

accumEvents :: Monad m => a -> R.Behavior t (a -> b -> a) -> R.Event t [b] -> m (R.Behavior t a)
accumEvents b bf e = accumB b (fol <$> bf <@> e)
  where fol f bs a = foldr (flip f) a bs

randomEvent :: Monad m => R.Event t (Rand StdGen a) -> m (R.Event t a)
randomEvent b = do
  initialStdGen <- liftIO $ newStdGen
  fst <$> mapAccum initialStdGen (runRand <$> b)

foldE :: (a -> b -> a) -> a -> [b] -> a
foldE x = foldr (flip x)

unionFirst :: [R.Event t a] -> R.Event t a
unionFirst = fmap getFirst . mconcat . fmap (fmap First)

noEvs :: (a -> b -> (a, [c])) -> a -> b -> a
noEvs f a b = fst $ f a b

foldEventEmitter :: (a -> b -> (a, [c])) -> a -> [b] -> (a, [c])
foldEventEmitter f a bs = foldr (flip $ evFolder f) (a, []) bs

evFolder :: (a -> b -> (a, [c])) -> (a, [c]) -> b -> (a, [c])
evFolder f (a, cs) b = fmap (<> cs) $ f a b

counter :: Monad m => R.Event t Int -> R.Event t Int -> m (Behavior Int)
counter eDelta eReset = accumB 0 $ unions [ (+) <$> eDelta, const <$> eReset ]

historical :: Monad m => a -> R.Event t (a -> a) -> R.Event t Int -> m (Behavior a)
historical initial eStep eChangeIndex =
  fst <$> historicalWithEvents initial (((,[]) .) <$> eStep) eChangeIndex

historicalWithEvents :: Monad m => a -> R.Event t (a -> (a,[b])) -> R.Event t Int -> m (R.Behavior t a, R.Event t [b])
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

{-
splitPair :: Event (a, b) -> (Event a, Event b)
splitPair e = (fst <$> e, snd <$> e)

foldEvents :: Behavior b -> (b -> a -> b) -> Event [a] -> Event b
foldEvents b h e = fmap (foldE h) b <@> e

foldEvents' :: Behavior (a -> b -> a) -> Behavior a -> Event [b] -> Event a
foldEvents' bf b e = foldE <$> bf <*> b <@> e

render :: DU.HasRenderState a => IORef a -> [Behavior (a -> DU.RenderTree)] -> Behavior (IO ())
render resRef renderFuncs = DU.render resRef <$> combineRenderFuncs renderFuncs
-}
