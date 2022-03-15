{-# LANGUAGE RecordWildCards #-}

module Hickory.FRP.CoreEvents where

import Data.Hashable (Hashable)
import Data.IORef (IORef, readIORef)
import Hickory.Input (TouchEvent(..), RawInput(..), Key)
import Hickory.Math.Vector (Scalar)
import Hickory.Utils.Utils (makeFPSTicker)
import Hickory.Types (Size)
import Reactive.Banana (Behavior, Event, mapAccum, filterE, filterJust, stepper)
import Reactive.Banana.Frameworks (fromAddHandler, newAddHandler, MomentIO, AddHandler, Handler, fromPoll)
import qualified Data.HashMap.Strict as HashMap
import Linear (V2(..))
import Data.Time (NominalDiffTime)
import Hickory.FRP.Combinators (unionFirst)

_1 :: (a, b, c) -> a
_1 (a,_,_) = a
_2 :: (a, b, c) -> b
_2 (_,b,_) = b
_3 :: (a, b, c) -> c
_3 (_,_,c) = c

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
  [ map (\(v,i)   -> Down i v) <$> eTouchesDown
  , map (\(_,v,i) -> Up i v)   <$> eTouchesUp
  , map (\(v,i)   -> Loc i v)  <$> eTouchesLoc
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
          InputKeyDown k -> fire (_1 keyPairs) k
          InputKeysHeld keymap -> fire (_2 keyPairs) keymap
          InputKeyUp k _ -> fire (_3 keyPairs) k
          InputTouchesDown touches -> fire (_1 touchPairs) touches
          InputTouchesUp   touches -> fire (_2 touchPairs) touches
          InputTouchesLoc  touches -> fire (_3 touchPairs) touches

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
