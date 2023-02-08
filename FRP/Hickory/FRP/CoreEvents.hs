{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Hickory.FRP.CoreEvents where

import Data.Hashable (Hashable)
import Data.IORef (IORef, readIORef)
import Hickory.Input (TouchEvent(..), RawInput(..), Key, TouchEventType (..), GamePad(..), gamePadButtonState, ButtonState (..), GamePadButton)
import Hickory.Math.Vector (Scalar)
import Hickory.Utils.Utils (makeFPSTicker)
import Hickory.Types (Size)
import Reactive.Banana (Behavior, Event, mapAccum, filterE, filterJust, stepper, whenE, mapAccum)
import Reactive.Banana.Frameworks (fromAddHandler, newAddHandler, MomentIO, AddHandler, Handler, fromPoll, MonadIO (..), mapEventIO)
import qualified Data.HashMap.Strict as HashMap
import Linear (V2(..))
import Data.Time (NominalDiffTime)
import Hickory.FRP.Combinators (unionFirst)
import qualified Data.HashMap.Strict as Map
import Data.Functor ((<&>))
import qualified Data.Enum.Set as ES
import Data.Word (Word32)

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


concatTouchEvents :: CoreEvents a -> Event [TouchEvent]
concatTouchEvents CoreEvents {..} = mconcat
  [ map (\(v,i)   -> TouchEvent i v Down) <$> eTouchesDown
  , map (\(dur,v,i) -> TouchEvent i v (Up dur))   <$> eTouchesUp
  , map (\(v,i)   -> TouchEvent i v Loc)  <$> eTouchesLoc
  ]

keyHandlers :: IO (HandlerPair a, HandlerPair b, HandlerPair c)
keyHandlers = (,,) <$> newAddHandler <*> newAddHandler <*> newAddHandler

mkKeyEvents :: (Ord b1, Fractional b1, Hashable k, Eq k) => (HandlerPair k, HandlerPair (HashMap.HashMap k b1), HandlerPair k) -> MomentIO (Event k, k -> Event k, Event k, k -> Behavior Bool)
mkKeyEvents (keyPair, keysHeldPair, keyUpPair) = do
  eKey    <- mkEvent keyPair
  eKeysHeld <- mkEvent keysHeldPair
  eKeyUp  <- mkEvent keyUpPair
  let -- keyDown k = filterE (==k) eKey
      keyDownOrHeld k = unionFirst [ filterE (==k) eKey
                                   , k <$ (filterE (>0.2) . filterJust $ HashMap.lookup k <$> eKeysHeld)
                                   ]
      -- keyUp k = filterE (==k) eKeyUp
  bKeysHeld <- stepper mempty eKeysHeld
  pure (eKey, keyDownOrHeld, eKeyUp, \k -> HashMap.member k <$> bKeysHeld)

data CoreEventGenerators a = CoreEventGenerators
  { renderEvent :: HandlerPair a
  , touchEvents :: ( HandlerPair [(V2 Scalar, Int)]
                   , HandlerPair [(Scalar, V2 Scalar, Int)]
                   , HandlerPair [(V2 Scalar, Int)])
  , keyEvents   :: (HandlerPair Key, HandlerPair (HashMap.HashMap Key Scalar), HandlerPair Key)
  , timeEvents  :: HandlerPair NominalDiffTime
  , windowSize  :: IORef (Size Int)
  , gamePadEvent :: HandlerPair (Int, GamePad)
  , gamePadConnectionEvent :: HandlerPair (Int, Bool)
  }

data CoreEvents a = CoreEvents
  { eRender       :: Event a
  , eTime         :: Event NominalDiffTime
  , keyDown       :: Event Key
  , keyDownOrHeld :: Key -> Event Key
  , keyUp         :: Event Key
  , keyHeldB      :: Key -> Behavior Bool
  , eTouchesDown  :: Event [Point]
  , eTouchesLoc   :: Event [Point]
  , eTouchesUp    :: Event [PointUp]
  , bGamePads     :: Behavior (Map.HashMap Int GamePad)
  , eGamePadPresses  :: Event (Int, ES.EnumSet GamePadButton)
  , eGamePadReleases :: Event (Int, ES.EnumSet GamePadButton)
  , eGamePadConnection :: Event (Int, Bool)
  , scrSizeB      :: Behavior (Size Int)
  , fpsB          :: Behavior Scalar
  , currentTimeB  :: Behavior NominalDiffTime
  , eNewTime      :: Event NominalDiffTime
  }

coreEventGenerators :: IO [RawInput] -> IO NominalDiffTime -> IORef (Size Int) -> IO (a -> IO (), CoreEventGenerators a)
coreEventGenerators inputPoller timePoller wSizeRef = do

  touchPairs <- touchHandlers
  keyPairs   <- keyHandlers
  timePair   <- newAddHandler
  renderPair <- newAddHandler
  gamePadPair <- newAddHandler
  gamePadConnectionPair <- newAddHandler

  let processor x = do
        fire renderPair x

        (inputPoller >>=) . mapM_ $ \case
          InputKeyDown k -> fire (_1 keyPairs) k
          InputKeysHeld keymap -> fire (_2 keyPairs) keymap
          InputKeyUp k _ -> fire (_3 keyPairs) k
          InputTouchesDown touches -> fire (_1 touchPairs) touches
          InputTouchesUp   touches -> fire (_2 touchPairs) touches
          InputTouchesLoc  touches -> fire (_3 touchPairs) touches
          InputGamePad ident gp    -> fire gamePadPair (ident, gp)
          InputGamePadConnection ident connected -> fire gamePadConnectionPair (ident, connected)

        timePoller >>= fire timePair

  pure (processor, CoreEventGenerators renderPair touchPairs keyPairs timePair wSizeRef gamePadPair gamePadConnectionPair)

mkCoreEvents :: CoreEventGenerators a -> MomentIO (CoreEvents a)
mkCoreEvents coreEvGens = do
  (eTouchesDown, eTouchesUp, eTouchesLoc)   <- mkTouchEvents . touchEvents $ coreEvGens
  (keyDown, keyDownOrHeld, keyUp, keyHeldB) <- mkKeyEvents   . keyEvents $ coreEvGens

  eGamePad <- mkEvent . gamePadEvent $ coreEvGens
  (eGamePadButton, bGamePads) <- mapAccum mempty $ eGamePad <&> \(idx, gp) acc ->
    case HashMap.lookup idx acc of
      Just oldGp ->
        let states = [minBound..maxBound] <&> \but -> (but, gamePadButtonState oldGp but, gamePadButtonState gp but)
            presses  = ES.fromFoldable . map (\(but, _, _) -> but) . flip filter states $ \(_, old, new) -> old == Released && new == Pressed
            releases = ES.fromFoldable . map (\(but, _, _) -> but) . flip filter states $ \(_, old, new) -> old == Pressed && new == Released

        in (((idx, presses), (idx, releases)), HashMap.insert idx gp acc)
      Nothing -> (((idx, ES.empty), (idx, ES.empty)), HashMap.insert idx gp acc)
  let eGamePadPresses = fst <$> eGamePadButton
      eGamePadReleases = snd <$> eGamePadButton

  eGamePadConnection <- mkEvent . gamePadConnectionEvent $ coreEvGens

  eTime   <- mkEvent . timeEvents $ coreEvGens
  eRender <- mkEvent . renderEvent $ coreEvGens

  scrSizeB <- fromPoll . readIORef . windowSize $ coreEvGens

  fpsTicker <- liftIO makeFPSTicker
  eFPS <- mapEventIO (const fpsTicker) eRender
  fpsB <- stepper 0 eFPS

  (eNewTime, currentTimeB) <- mapAccum 0 $ (\timeDelt x -> (timeDelt + x, timeDelt + x)) <$> eTime

  pure $ CoreEvents { .. }

maskCoreEvents :: Behavior Bool -> CoreEvents a -> CoreEvents a
maskCoreEvents switch ce@CoreEvents {..} = ce
  { eRender = whenE switch eRender
  , eTime = whenE switch eTime
  , keyDown = whenE switch keyDown
  , keyDownOrHeld = whenE switch . keyDownOrHeld
  , keyUp = whenE switch keyUp
  , eTouchesDown = whenE switch eTouchesDown
  , eTouchesLoc = whenE switch eTouchesLoc
  , eTouchesUp = whenE switch eTouchesUp
  , eGamePadPresses  = whenE switch eGamePadPresses
  , eGamePadReleases = whenE switch eGamePadReleases
  , eNewTime = whenE switch eNewTime
  }
