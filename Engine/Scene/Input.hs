module Engine.Scene.Input where

import Math.Vector
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable

-- key is the type used by your platform to represent a key. e.g. GLFW's Key type
data RawInput = InputTouchesDown [(V2,Int)]
              | InputTouchesUp [(Double,V2,Int)]
              | InputTouchesLoc [(V2,Int)]
              | InputKeyDown Key
              | InputKeyUp Key Scalar
              | InputKeysHeld (HashMap.HashMap Key Double)
              deriving (Show)

data Key = Key'Unknown
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
         deriving (Show, Eq, Ord, Enum)

instance Data.Hashable.Hashable Key where
      hashWithSalt s k = hashWithSalt s (fromEnum k)
