module Engine.Scene.Input where

import Math.Vector
import qualified Graphics.UI.GLFW as GLFW

data RawInput = InputTouchDown V2 Int
             | InputTouchUp V2 Int 
             | InputTouchLoc V2 Int 
             | InputKeyDown GLFW.Key
             | InputKeyUp GLFW.Key
             deriving (Show)

data Input ev = Input {
           inputEvents :: [ev]
           } deriving Show
