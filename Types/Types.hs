module Types.Types
   (
   Size(..),
   FSize,
   nullSize,
   mkSize,
   aspectRatio,
   screenPos,
   RelativeScalar(..),
   RelativeVec(..),
   beg, end, center,
   fracSize,
   viewportFromSize,
   Rect(..),
   posInRect,
   transform,
   transform2,
   RelativeRect(..),
   transformRect,
   convertSize,
   screenCenter,
   addConst,
   relativePosInRect
   ) where

import Math.Vector

data Size a = Size a a deriving (Show)

convertSize :: (Real a, Fractional b) => Size a -> Size b
convertSize (Size a b) = Size (realToFrac a) (realToFrac b)

type FSize = Size Float

screenCenter :: Real a => Size a -> V2 Scalar
screenCenter (Size w h) = v2 (realToFrac w / 2) (realToFrac h / 2)

nullSize :: Num a => Size a
nullSize = Size 0 0

mkSize :: Num a => a -> Size a
mkSize a = Size a a

aspectRatio :: (Real a, Fractional b) => Size a -> b
aspectRatio (Size w h) = w' / h'
    where (Size w' h') = Size (realToFrac w) (realToFrac h)

viewportFromSize :: Integral a => Size a -> V4 a
viewportFromSize (Size w h) = v4 0 0 (fromIntegral w) (fromIntegral h)

fracSize :: (Real a, Fractional b) => Size a -> Size b
fracSize (Size w h) = Size (realToFrac w) (realToFrac h)

data Rect = Rect (V2 Scalar) (Size Scalar) deriving (Show)

data RelativeRect a b = RRect (RelativeVec a b) (RelativeVec a b)

data RelativeVec a b = RVec (RelativeScalar a b) (RelativeScalar a b)

transformRect :: (Real b, Real a) => RelativeRect Scalar b -> Size a -> Rect
transformRect (RRect (RVec rx ry) (RVec rw rh)) (Size w h) =
        Rect (v2 (transform rx w) (transform ry h)) (Size (transform rw w) (transform rh h))

posInRect :: V2 Scalar -> Rect -> Bool
posInRect (V2 px py) (Rect (V2 ox oy) (Size w h)) =
        ((abs (ox - px)) < (w/2)) && ((abs (oy - py)) < (h/2))

relativePosInRect :: V2 Scalar -> Rect -> Maybe (V2 Scalar)
relativePosInRect (V2 px py) (Rect (V2 ox oy) (Size w h)) =
        let rx = (px - ox + (w/2)) / w
            ry = (py - oy + (h/2)) / h
            in if (rx >= 0 && rx <= 1) && (ry >= 0 && ry <= 1)
                   then Just $ v2 rx ry
                   else Nothing

data RelativeScalar fract offset = RScal fract offset

transform :: (Fractional a, Real b, Real c) => RelativeScalar a b -> c -> a
transform (RScal fract offset) val = fract * (realToFrac val) + (realToFrac offset)

beg :: Num b => a -> RelativeScalar b a
beg a = RScal 0 a
end :: (Num b, Num a) => a -> RelativeScalar b a
end a = RScal 1 (negate a)
center :: Fractional b => a -> RelativeScalar b a
center a = RScal 0.5 a

addConst :: Num a => RelativeScalar b a -> a -> RelativeScalar b a
addConst (RScal fr con) plusConst = RScal fr (con + plusConst)

screenPos :: (Real a, Real b) => Size a -> RelativeVec Scalar Scalar -> V3 Scalar
screenPos (Size w h) (RVec xl yl) = v3 (transform xl w) (transform yl h) 0

transform2 :: (Real a, Real b) => Size a -> RelativeVec Scalar Scalar -> V2 Scalar
transform2 (Size w h) (RVec xl yl) = v2 (transform xl w) (transform yl h)
