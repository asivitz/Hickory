module Freecell.Events where

import Engine.Input

data InputEvent = RawEvent RawInput
                | NewGame deriving Show

