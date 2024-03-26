{-# LANGUAGE DeriveGeneric, OverloadedLabels, StrictData, OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}

module Hickory.Input where

import Linear (V2(..), zero)
import Hickory.Math.Vector
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import GHC.Generics (Generic)
import Control.Lens (over)
import Data.Generics.Labels ()

import qualified Data.Enum.Set as E
import Data.Word (Word32)
import Data.Time (NominalDiffTime, getCurrentTime, diffUTCTime)
import Data.IORef (atomicModifyIORef, newIORef, atomicModifyIORef')
import Data.Foldable (foldl')
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.WideWord.Word128 (Word128)
import Data.HashMap.Strict (HashMap)

type Point = (V2 Scalar, Int) -- Location, Ident

data PointUp = PointUp
 { duration :: Scalar
 , location :: V2 Scalar
 , origLocation :: V2 Scalar
 , ident :: Int
 } deriving (Show)

-- key is the type used by your platform to represent a key. e.g. GLFW's Key type
data RawInput = InputTouchesDown [Point]
              | InputTouchesUp [PointUp]
              | InputTouchesLoc [(V2 Scalar,Int)]
              | InputKeyDown Key
              | InputKeyUp Key Scalar
              | InputKeysHeld (HashMap.HashMap Key Scalar)
              | InputGamePad Int GamePad -- GamePad Index, GamePad
              | InputGamePadConnection Int Bool -- GamePad Index, True == connected
              deriving (Show)

data ButtonState = Pressed | Released
  deriving (Eq, Show)

data GamePad = GamePad
  { leftStick    :: V2 Scalar
  , rightStick   :: V2 Scalar
  , leftTrigger  :: Scalar
  , rightTrigger :: Scalar
  , a            :: ButtonState
  , b            :: ButtonState
  , x            :: ButtonState
  , y            :: ButtonState
  , leftBumper   :: ButtonState
  , rightBumper  :: ButtonState
  , back         :: ButtonState
  , start        :: ButtonState
  , guide        :: ButtonState
  , leftThumb    :: ButtonState
  , rightThumb   :: ButtonState
  , dpadUp       :: ButtonState
  , dpadRight    :: ButtonState
  , dpadDown     :: ButtonState
  , dpadLeft     :: ButtonState
  , cross        :: ButtonState
  , circle       :: ButtonState
  , square       :: ButtonState
  , triangle     :: ButtonState
  } deriving (Show, Generic)

emptyGamePad :: GamePad
emptyGamePad = GamePad {..}
  where
  leftStick = zero
  rightStick = zero
  leftTrigger = 0
  rightTrigger = 0
  a = Released
  b = Released
  x = Released
  y = Released
  leftBumper = Released
  rightBumper = Released
  back = Released
  start = Released
  guide = Released
  leftThumb = Released
  rightThumb = Released
  dpadUp = Released
  dpadDown = Released
  dpadRight = Released
  dpadLeft = Released
  cross = Released
  circle = Released
  square = Released
  triangle = Released

data GamePadButton
  = A
  | B
  | X
  | Y
  | LeftBumper
  | RightBumper
  | Back
  | Start
  | Guide
  | LeftThumb
  | RightThumb
  | DPadUp
  | DPadRight
  | DPadDown
  | DPadLeft
  | Cross
  | Circle
  | Square
  | Triangle
  | LeftStickUp
  | LeftStickRight
  | LeftStickDown
  | LeftStickLeft
  | RightStickUp
  | RightStickRight
  | RightStickDown
  | RightStickLeft
  deriving (Enum, Bounded, Eq, Show)

gamePadButtonState :: GamePad -> GamePadButton -> ButtonState
gamePadButtonState gp = \case
  A -> gp.a
  B -> gp.b
  X -> gp.x
  Y -> gp.y
  LeftBumper -> gp.leftBumper
  RightBumper -> gp.rightBumper
  Back -> gp.back
  Start -> gp.start
  Guide -> gp.guide
  LeftThumb -> gp.leftThumb
  RightThumb -> gp.rightThumb
  DPadUp -> gp.dpadUp
  DPadRight -> gp.dpadRight
  DPadDown -> gp.dpadDown
  DPadLeft -> gp.dpadLeft
  Cross -> gp.cross
  Circle -> gp.circle
  Square -> gp.square
  Triangle -> gp.triangle
  LeftStickUp    -> let V2 _x y = gp.leftStick in if y < -0.8 then Pressed else Released
  LeftStickRight -> let V2 x _y = gp.leftStick in if x > 0.8 then Pressed else Released
  LeftStickDown  -> let V2 _x y = gp.leftStick in if y > 0.8 then Pressed else Released
  LeftStickLeft  -> let V2 x _y = gp.leftStick in if x < -0.8 then Pressed else Released
  RightStickUp    -> let V2 _x y = gp.rightStick in if y < -0.8 then Pressed else Released
  RightStickRight -> let V2 x _y = gp.rightStick in if x > 0.8 then Pressed else Released
  RightStickDown  -> let V2 _x y = gp.rightStick in if y > 0.8 then Pressed else Released
  RightStickLeft  -> let V2 x _y = gp.rightStick in if x < -0.8 then Pressed else Released

instance E.AsEnumSet GamePadButton where
  type EnumSetRep GamePadButton = Word32

instance E.AsEnumSet Key where
  type EnumSetRep Key = Word128

data Key =
    Key'Unknown
  | Key'Space
  | Key'Apostrophe
  | Key'Comma
  | Key'Minus
  | Key'Period
  | Key'Slash
  | Key'0
  | Key'1
  | Key'2
  | Key'3
  | Key'4
  | Key'5
  | Key'6
  | Key'7
  | Key'8
  | Key'9
  | Key'Semicolon
  | Key'Equal
  | Key'A
  | Key'B
  | Key'C
  | Key'D
  | Key'E
  | Key'F
  | Key'G
  | Key'H
  | Key'I
  | Key'J
  | Key'K
  | Key'L
  | Key'M
  | Key'N
  | Key'O
  | Key'P
  | Key'Q
  | Key'R
  | Key'S
  | Key'T
  | Key'U
  | Key'V
  | Key'W
  | Key'X
  | Key'Y
  | Key'Z
  | Key'LeftBracket
  | Key'Backslash
  | Key'RightBracket
  | Key'GraveAccent
  | Key'World1
  | Key'World2
  | Key'Escape
  | Key'Enter
  | Key'Tab
  | Key'Backspace
  | Key'Insert
  | Key'Delete
  | Key'Right
  | Key'Left
  | Key'Down
  | Key'Up
  | Key'PageUp
  | Key'PageDown
  | Key'Home
  | Key'End
  | Key'CapsLock
  | Key'ScrollLock
  | Key'NumLock
  | Key'PrintScreen
  | Key'Pause
  | Key'F1
  | Key'F2
  | Key'F3
  | Key'F4
  | Key'F5
  | Key'F6
  | Key'F7
  | Key'F8
  | Key'F9
  | Key'F10
  | Key'F11
  | Key'F12
  | Key'F13
  | Key'F14
  | Key'F15
  | Key'F16
  | Key'F17
  | Key'F18
  | Key'F19
  | Key'F20
  | Key'F21
  | Key'F22
  | Key'F23
  | Key'F24
  | Key'F25
  | Key'Pad0
  | Key'Pad1
  | Key'Pad2
  | Key'Pad3
  | Key'Pad4
  | Key'Pad5
  | Key'Pad6
  | Key'Pad7
  | Key'Pad8
  | Key'Pad9
  | Key'PadDecimal
  | Key'PadDivide
  | Key'PadMultiply
  | Key'PadSubtract
  | Key'PadAdd
  | Key'PadEnter
  | Key'PadEqual
  | Key'LeftShift
  | Key'LeftControl
  | Key'LeftAlt
  | Key'LeftSuper
  | Key'RightShift
  | Key'RightControl
  | Key'RightAlt
  | Key'RightSuper
  | Key'Menu
  deriving (Enum, Eq, Ord, Read, Show)

instance Data.Hashable.Hashable Key where
      hashWithSalt s k = hashWithSalt s (fromEnum k)

type TouchIdent = Int

data TouchEvent = TouchEvent
  { touchIdent :: TouchIdent
  , loc        :: V2 Scalar
  , eventType  :: TouchEventType
  } deriving (Show, Generic)

data TouchEventType
  = Up Scalar (V2 Scalar) -- Touch ends (w/ duration and initial position)
  | Down            -- Touch begins
  | Loc             -- Touch is moved by user
  deriving (Show, Generic)

mapTouchEvent :: (V2 Scalar -> V2 Scalar) -> TouchEvent -> TouchEvent
mapTouchEvent = over #loc

makeTimePoller :: IO (IO NominalDiffTime)
makeTimePoller = do
  initial_time <- getCurrentTime
  time <- newIORef initial_time
  pure do
    new_time <- getCurrentTime
    atomicModifyIORef time (\prev_time -> (new_time, min 0.1 (diffUTCTime new_time prev_time)))

makeInputPoller :: ((RawInput -> IO ()) -> IO (IO ())) -> IO (IO [RawInput])
makeInputPoller inputSetup = do
  is <- newIORef []
  stepInp <- inputSetup (addRawInput is)

  pure do
    stepInp
    atomicModifyIORef is ([],)
  where
  addRawInput stream event = atomicModifyIORef stream (\evs -> (evs ++ [event], ()))

data InputFrame = InputFrame
  { delta              :: NominalDiffTime
  , pressedKeys        :: E.EnumSet Key
  , releasedKeys       :: E.EnumSet Key
  , heldKeys           :: E.EnumSet Key
  , touchesDown        :: [Point]
  , touchesUp          :: [PointUp]
  , touchesLoc         :: [Point]
  , gamePadPressed     :: HashMap Int [E.EnumSet GamePadButton]
  , gamePadReleased    :: HashMap Int [E.EnumSet GamePadButton]
  , gamePad            :: HashMap Int GamePad -- GamePad Index, GamePad
  , gamePadConnections :: [(Int, Bool)]    -- GamePad Index, True == connected
  , frameNum           :: Word -- Needed to handle switchover of interpolation from
                               -- one frame to the next
  }
  deriving (Show, Generic)

instance Semigroup InputFrame where
  a <> b = InputFrame {..}
    where
    delta          = a.delta + b.delta
    pressedKeys    = a.pressedKeys `E.union` b.pressedKeys
    releasedKeys   = a.releasedKeys `E.union` b.releasedKeys
    heldKeys       = (b.heldKeys `E.union` a.pressedKeys) E.\\ a.releasedKeys
    touchesDown    = a.touchesDown <> b.touchesDown
    touchesUp      = a.touchesUp   <> b.touchesUp
    touchesLoc     = a.touchesLoc  <> b.touchesLoc
    gamePadPressed     = HashMap.unionWith (++) a.gamePadPressed b.gamePadPressed
    gamePadReleased    = HashMap.unionWith (++) a.gamePadReleased b.gamePadReleased
    gamePad            = HashMap.union a.gamePad b.gamePad
    gamePadConnections = a.gamePadConnections <> b.gamePadConnections
    frameNum       = b.frameNum

instance Monoid InputFrame where
  mempty = InputFrame {..}
    where
    delta = 0
    pressedKeys = E.empty
    releasedKeys = E.empty
    heldKeys = E.empty
    touchesDown = mempty
    touchesUp = mempty
    touchesLoc = mempty
    gamePadPressed = mempty
    gamePadReleased = mempty
    gamePad = mempty
    gamePadConnections = mempty
    frameNum = 0

inputFrameBuilder :: IO ([RawInput] -> IO InputFrame)
inputFrameBuilder = do
  timePoller     <- makeTimePoller
  heldKeysRef    <- newIORef E.empty
  -- heldTouchesRef <- newIORef mempty
  gamepadsRef    <- newIORef mempty

  pure $ \newInputs -> do
    delta <- timePoller

    let pressedKeys  = foldl' (flip E.insert) E.empty $ [k | InputKeyDown k <- newInputs]
        releasedKeys = foldl' (flip E.insert) E.empty $ [k | InputKeyUp k _ <- newInputs]
    heldKeys <- atomicModifyIORef' heldKeysRef \curKeys -> (\a -> (a,a)) $ E.union curKeys pressedKeys E.\\ releasedKeys

    let gamePadConnections = [(i,b) | InputGamePadConnection i b <- newInputs]
        newGamepadStates = HashMap.fromList [(i, gp) | InputGamePad i gp <- newInputs]
    (gamePad, gamePadPressed, gamePadReleased) <- atomicModifyIORef' gamepadsRef \curGamePads ->
      let newGamePad = HashMap.union newGamepadStates curGamePads
          states oldGp newGp = [minBound..maxBound] <&> \but -> (but, gamePadButtonState oldGp but, gamePadButtonState newGp but)
          newPressed = flip HashMap.mapWithKey newGamepadStates \i newGP ->
            let oldGP = fromMaybe emptyGamePad $ HashMap.lookup i curGamePads
            in pure $ E.fromFoldable . map (\(but, _, _) -> but) . flip filter (states oldGP newGP) $ \(_, old, new) -> old == Released && new == Pressed
          newReleased = flip HashMap.mapWithKey newGamepadStates \i newGP ->
            let oldGP = fromMaybe emptyGamePad $ HashMap.lookup i curGamePads
            in pure $ E.fromFoldable . map (\(but, _, _) -> but) . flip filter (states oldGP newGP) $ \(_, old, new) -> old == Pressed && new == Released
      in (newGamePad, (newGamePad, newPressed, newReleased))

    let touchesDown = [p | InputTouchesDown ps <- newInputs, p <- ps]
        touchesUp   = [p | InputTouchesUp ps <- newInputs, p <- ps]
        touchesLoc  = [p | InputTouchesLoc ps <- newInputs, p <- ps]

        frameNum = 0
    -- touchesLoc <- atomicModifyIORef' heldTouchesRef \curTouches ->
    --   let newTouches =
    --           flip (foldl' (\m PointUp {..} -> HashMap.delete ident m)) touchesUp
    --         . flip (foldl' (\m (v,i) -> HashMap.insert i v m)) touchesDown
    --         $ curTouches
    --   in (newTouches, swap <$> HashMap.toList newTouches)

    pure InputFrame {..}
