module Menus.Construction where

import Utils.Utils
import Utils.Projection
import Engine.Scene.Input
import Types.Types
import Engine.Scene.Scene
import Menus.Menus
import Data.Maybe
import Math.Vector

{-import Types.Color-}

data Interval a = Interval a a

intervalIndices :: [Double]
intervalIndices = [5, 3, 1, 4, 3, 0]

intervals :: [Interval Double]
intervals = map (\i -> Interval (i * 0.1) (i * 0.1 + 0.5)) intervalIndices

pickInterval idx = intervals !! (idx `mod` 6)

constrainInterval :: Double -> Int -> Double
constrainInterval fraction idx = case (pickInterval idx) of
                                     Interval low high -> rlerpClamp fraction low high

slide :: Fractional a => Bool -> RelativeScalar a b -> RelativeScalar a b
slide True (RScal frac offset) = (RScal (frac * 0.5) offset)
slide False (RScal frac offset) = (RScal ((2 - frac) * 0.5) offset)


processMenuStack :: RenderInfo -> V2 -> TransitionStack (MenuScreen ie mdc) -> Maybe (TransitionStack (MenuScreen ie mdc), [ie])
processMenuStack renderinfo@(RenderInfo _ ss _) pos transitionStk =
        let unproj = unproject pos (-5) renderinfo in
            case incomingScreen transitionStk of
                Just (MenuScreen elements _) -> 
                    let acts = listToMaybe $ mapMaybe (\(UIElement mbutton _) -> 
                            case mbutton of
                                Just (Button rrect actions) -> if (posInRect (v3tov2 unproj) (transformRect rrect ss)) 
                                                                then Just actions
                                                                else Nothing
                                Nothing -> Nothing) elements
                        in case acts of
                               Just (ies, action) -> Just (maybe transitionStk (\a -> a transitionStk ) action, ies)
                               Nothing -> Nothing
                Nothing -> Nothing
