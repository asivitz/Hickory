module Engine.Input where

import Math.Vector
import Math.Matrix
import qualified Graphics.UI.GLFW as GLFW

data InputEv = InputTouchDown V2 Int
             | InputTouchUp V2 Int 
             | InputTouchLoc V2 Int 
             | InputKeyDown GLFW.Key
             | InputKeyUp GLFW.Key
             deriving (Show)

data Input = Input {
           inputEvents :: [InputEv]
           } deriving Show
