module Freecell.Utils where

import Data.Maybe
import Data.List
import Graphics.GLUtils

data Card = Card Int TexID deriving (Show)

instance Eq Card where
        (Card x _) == (Card y _) = x == y

data FreecellGame = FreecellGame [Card] [Card] deriving Show

cardDepth :: FreecellGame -> Card -> Int
cardDepth (FreecellGame stack1 stack2) inCard = case firstIndexFound inCard [stack1, stack2] of
        Nothing -> (-1)
        Just idx -> idx
    where firstIndexFound card cardLst = listToMaybe . mapMaybe (elemIndex card) $ cardLst

