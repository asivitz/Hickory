{-# LANGUAGE DeriveGeneric, OverloadedLabels, StrictData, OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}

module Hickory.Input where

import Linear (V2(..))
import Hickory.Math.Vector
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import GHC.Generics (Generic)
import Control.Lens (over)
import Data.Generics.Labels ()

import Data.Enum.Set as E
import Data.Word (Word32)

-- key is the type used by your platform to represent a key. e.g. GLFW's Key type
data RawInput = InputTouchesDown [(V2 Scalar,Int)]
              | InputTouchesUp [(Scalar,V2 Scalar,Int)]
              | InputTouchesLoc [(V2 Scalar,Int)]
              | InputKeyDown Key
              | InputKeyUp Key Scalar
              | InputKeysHeld (HashMap.HashMap Key Scalar)
              | InputGamePad Int GamePad -- GamePad Index, GamePad
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
  } deriving Show

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
  deriving (Enum, Bounded, Eq)

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

instance E.AsEnumSet GamePadButton where
  type EnumSetRep GamePadButton = Word32

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
  = Up Scalar       -- Touch ends (w/ duration)
  | Down            -- Touch begins
  | Loc             -- Touch is moved by user
  deriving (Show, Generic)

mapTouchEvent :: (V2 Scalar -> V2 Scalar) -> TouchEvent -> TouchEvent
mapTouchEvent = over #loc
