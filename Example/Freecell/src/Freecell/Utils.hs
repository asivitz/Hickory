module Freecell.Utils where

import Data.Maybe
import Data.List

data Card = Card Int deriving (Show, Eq)

data FreecellGame = FreecellGame [Card] [Card] deriving Show

cardDepth :: FreecellGame -> Card -> Int
cardDepth (FreecellGame stack1 stack2) card = case firstIndexFound card [stack1, stack2] of
        Nothing -> (-1)
        Just idx -> idx
    where firstIndexFound card cardLst = listToMaybe . mapMaybe (elemIndex card) $ cardLst

