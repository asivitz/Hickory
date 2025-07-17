module Hickory.UI where

import Linear (V2(..))
import Hickory.Types (Size(..))

bottomRight :: (Real a1, Fractional a2) => a2 -> a2 -> Size a1 -> V2 a2
bottomRight x y (Size w h) = V2 (realToFrac w - x) (realToFrac h - y)

bottomLeft :: (Real a1, Fractional a2) => a2 -> a2 -> Size a1 -> V2 a2
bottomLeft x y (Size _w h) = V2 x (realToFrac h - y)

topRight :: (Real a1, Fractional a2) => a2 -> a2 -> Size a1 -> V2 a2
topRight x y (Size w _h) = V2 (realToFrac w - x) y

topLeft :: (Real a1, Fractional a2) => a2 -> a2 -> Size a1 -> V2 a2
topLeft x y (Size _w _h) = V2 x y

topMiddle :: (Fractional a1, Real a2) => a1 -> Size a2 -> V2 a1
topMiddle y (Size w _h) = V2 (realToFrac w/2) y

bottomMiddle :: (Fractional a1, Real a2) => a1 -> Size a2 -> V2 a1
bottomMiddle y (Size w h) = V2 (realToFrac w/2) (realToFrac h - y)

middle :: (Fractional b, Real a) => Size a -> V2 b
middle (Size w h) = V2 (realToFrac w/2) (realToFrac h/2)

right :: (Real a1, Fractional a2) => Size a1 -> V2 a2
right (Size w _h) = V2 (realToFrac w) 0

middleY :: (Real a1, Fractional a2) => Size a1 -> V2 a2
middleY (Size _w h) = V2 0 (realToFrac h/2)

middleX :: (Real a1, Fractional a2) => Size a1 -> V2 a2
middleX (Size w _h) = V2 (realToFrac w/2) 0
