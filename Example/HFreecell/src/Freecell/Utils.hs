{-# LANGUAGE Rank2Types #-}
-- {-# LANGUAGE TupleSections #-}

module Freecell.Utils where

import Data.Maybe
import Data.List
import FreeCell
import Math.Vector
import Debug.Trace
import Utils.Utils

class CardStack a => VisibleCardStack a where
        depthIfVisible :: Card -> a -> Maybe Int
        visibleLength :: a -> Int

instance VisibleCardStack Foundation where
        depthIfVisible card (Foundation []) = Nothing
        depthIfVisible card (Foundation (x:xs)) = if x == card then Just 0 else Nothing
        visibleLength _ = 0

instance VisibleCardStack Cascade where
        depthIfVisible card (Cascade lst) = elemIndex card lst
        visibleLength (Cascade lst) = length lst - 1

instance VisibleCardStack Freecell where
        depthIfVisible card (Freecell Nothing) = Nothing
        depthIfVisible card (Freecell (Just a)) = if a == card then Just 0 else Nothing
        visibleLength _ = 0

findFirstBoardValue :: Board -> (forall a. VisibleCardStack a => a -> Maybe b) -> Maybe (Int, b)
findFirstBoardValue (Board cs fd fc) f = listToMaybe . catMaybes $ [
                                                                   listToMaybe . mapMaybe (\(s,n) -> maybe Nothing (\x -> Just (n,x)) (f s)) $ zip cs [0..], 
                                                                   listToMaybe . mapMaybe (\(s,n) -> maybe Nothing (\x -> Just (n,x)) (f s)) $ zip fd [8..], 
                                                                   listToMaybe . mapMaybe (\(s,n) -> maybe Nothing (\x -> Just (n,x)) (f s)) $ zip fc [12..]
                                                                   ]

cardDepth :: Board -> Card -> Maybe Int
cardDepth board card = fmap snd $ findFirstBoardValue board (depthIfVisible card)

pileIndexForCard :: Board -> Card -> Maybe Int
pileIndexForCard board card = fmap fst $ findFirstBoardValue board $ (card `elemIndex`) . getCards

pileLocationForCard :: Board -> Card -> Maybe (Int, Int)
pileLocationForCard board card = findFirstBoardValue board $ (\stk -> fmap (visibleLength stk -) (card `depthIfVisible` stk ))

{-allCards :: Board -> [Card]-}
{-allCards (Board cs fd fc) = concatMap getCards fc ++ concatMap getCards cs ++ concatMap getCards fd-}

allCards = [Card Ace Heart .. Card King Spade]

pilePositions = [
                v2 1.5 4,
                v2 2.5 4,
                v2 3.5 4,
                v2 4.5 4,
                v2 5.5 4,
                v2 6.5 4,
                v2 7.5 4,
                v2 8.5 4,
                v2 6 6,
                v2 7 6,
                v2 8 6,
                v2 9 6,
                v2 1 6,
                v2 2 6,
                v2 3 6,
                v2 4 6]

currentLocation board card = 
        case pileIndexForCard board card of
            Nothing -> error "Can't find card."
            Just a | a < 8 -> CascadesSource
            Just a | a < 12 -> Foundations
            Just a -> FreeCell (a - 12)


forceUntilStable board = case length possible of
                             0 -> board
                             1 -> forceUntilStable (gameBoard . head $ possible)
                             _ -> board
        where possible = allPermissable board

posForCard :: Board -> Card -> V3
posForCard board card = case pileLocationForCard board card of
                            Just (index, depth) -> flip v2tov3 (-5) $ (pilePositions !! index) + v2 0 ((-0.35) * (realToFrac depth))
                            Nothing -> v3 0 0 0

moveCard board card location = let move = Move card (currentLocation board card) location in
    forceUntilStable $ moveBoard board move

moveBoard board move = case listToMaybe . filter ((==move) . sourceMove) $ allPermissable board of
                           Nothing -> trace ("Invalid Move: " ++ (show move)) $ board
                           Just a -> gameBoard  a

dropLocationForPos board pos = case nearestPileLoc of
                                   a | a < 8 -> Cascades a
                                   a | a < 12 -> Foundations
                                   a -> FreeCell (a - 12)
    where nearestPileLoc = snd . head . sortOn (\(p, i) -> vmag (pos - p)) $ zip pilePositions [0..]

