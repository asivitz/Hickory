module Freecell.Events where

import Engine.Scene.Input

data InputEvent = RawEvent RawInput
                | NewGame deriving Show

