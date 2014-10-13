module Engine.Scene.Input where

import Math.Vector

data RawInput key = InputTouchDown V2 Int
             | InputTouchUp Scalar V2 Int 
             | InputTouchLoc V2 Int 
             | InputKeyDown key
             | InputKeyUp key
             deriving (Show)

data Input ev = Input {
           inputEvents :: [ev]
           } deriving Show
