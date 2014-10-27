module Engine.Scene.Input where

import Math.Vector
import qualified Data.HashMap.Strict as HashMap

-- key is the type used by your platform to represent a key. e.g. GLFW's Key type
data RawInput = InputTouchesDown [(V2,Int)]
              | InputTouchesUp [(Double,V2,Int)]
              | InputTouchesLoc [(V2,Int)]
              | InputKeyDown Key
              | InputKeyUp Key Scalar
              | InputKeysHeld (HashMap.HashMap Key Double)
              deriving (Show)

data Key = KeyA
         | KeyB
         | KeyC
         | KeyD
         | KeyE
         | KeyF
         | KeyG
         | KeyH
         | KeyI
         | KeyJ
         | KeyK
         | KeyL
         | KeyM
         | KeyN
         | KeyO
         | KeyP
         | KeyQ
         | KeyR
         | KeyS
         | KeyT
         | KeyU
         | KeyV
         | KeyW
         | KeyX
         | KeyY
         | KeyZ
         deriving Show

