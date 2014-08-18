module Types.Types 
   ( 
   Size(..),
   FSize,
   nullSize,
   aspectRatio,
   screenPos,
   RelativePos(..),
   beg, end, center,
   fracSize,
   viewportFromSize,
   Rect(..),
   posInRect,
   transform,
   RelativeRect(..),
   transformRect,
   convertSize
   ) where

import Math.Vector

data Size a = Size a a deriving (Show)

convertSize :: (Real a, Fractional b) => Size a -> Size b
convertSize (Size a b) = Size (realToFrac a) (realToFrac b)

type FSize = Size Float

nullSize :: Num a => Size a
nullSize = (Size 0 0)

aspectRatio :: (Real a, Fractional b) => Size a -> b
aspectRatio (Size w h) = w' / h'
    where (Size w' h') = (Size (realToFrac w) (realToFrac h))

viewportFromSize :: Integral a => Size a -> V4
viewportFromSize (Size w h) = v4 0 0 (fromIntegral w) (fromIntegral h)

fracSize :: (Real a, Fractional b) => Size a -> Size b
fracSize (Size w h) = Size (realToFrac w) (realToFrac h)

data Rect = Rect V2 (Size Scalar) deriving (Show)

data RelativeRect a b = RRect (RelativePos a b, RelativePos a b) (RelativePos a b, RelativePos a b)

transformRect :: (Real b, Real a) => RelativeRect Scalar b -> Size a -> Rect
transformRect (RRect (rx, ry) (rw, rh)) (Size w h) =
        Rect (v2 (transform rx w) (transform ry h)) (Size (transform rw w) (transform rh h))

posInRect :: V2 -> Rect -> Bool
posInRect (Vector2 px py) (Rect (Vector2 ox oy) (Size w h)) =
        ((abs (ox - px)) < (w/2)) && ((abs (oy - py)) < (h/2))

data RelativePos a b = RPos a b

transform :: (Fractional a, Real b, Real c) => RelativePos a b -> c -> a
transform (RPos fract offset) val = fract * (realToFrac val) + (realToFrac offset)

beg :: Num b => a -> RelativePos b a
beg a = RPos 0 a
end :: (Num b, Num a) => a -> RelativePos b a
end a = RPos 1 (negate a)
center :: Fractional b => a -> RelativePos b a
center a = RPos 0.5 a

screenPos :: (Real a, Real b) => Size a -> RelativePos Scalar b -> RelativePos Scalar b -> V3
screenPos (Size w h) yl xl = v3 (transform xl w) (transform yl h) 0
