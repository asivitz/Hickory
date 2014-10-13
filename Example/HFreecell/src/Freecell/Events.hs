module Freecell.Events where

import Engine.Scene.Input
import qualified Graphics.UI.GLFW as GLFW

data InputEvent = RawEvent (RawInput GLFW.Key)
                | NewGame
                | WonGame
                | LostGame deriving Show

