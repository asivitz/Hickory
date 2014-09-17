module Menus.Construction where

import Utils.Utils
import Types.Types
{-import Types.Color-}
{-import Menus.Menus-}

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
