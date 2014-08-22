module Freecell.Utils where

import Data.Maybe
import Data.List
import Graphics.GLUtils

data Card = Card Int TexID deriving (Show)

instance Eq Card where
        (Card x _) == (Card y _) = x == y

data Pile = Stack [Card]
          | Fold [Card]
          | Single Card deriving Show

depthIfVisible card (Stack lst) = case listToMaybe lst of
                                      Nothing -> Nothing
                                      Just a -> if a == card
                                                    then Just 0
                                                    else Nothing

depthIfVisible card (Fold lst) = elemIndex card lst

depthIfVisible card (Single a) = if a == card then Just 0 else Nothing

data FreecellGame = FreecellGame Pile Pile deriving Show

allPiles (FreecellGame s1 s2) = [s1,s2]

cardDepth :: FreecellGame -> Card -> Maybe Int
cardDepth game card = listToMaybe . mapMaybe (depthIfVisible card) $ (allPiles game)
