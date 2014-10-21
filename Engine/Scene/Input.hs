module Engine.Scene.Input where

import Math.Vector

-- key is the type used by your platform to represent a key. e.g. GLFW's Key type
data RawInput key = InputTouchesDown [(V2,Int)]
                  | InputTouchesUp [(Scalar,V2,Int)]
                  | InputTouchesLoc [(V2,Int)]
                  | InputKeyDown key
                  | InputKeyUp key
                  deriving (Show)
