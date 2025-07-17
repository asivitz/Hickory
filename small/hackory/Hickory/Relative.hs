module Hickory.Relative
   (
   Size(..),
   screenPos,
   RelativeScalar(..),
   RelativeVec(..),
   beg, end, center,
   transform,
   transform2,
   RelativeRect(..),
   transformRect,
   addConst
   ) where

import Hickory.Math.Vector
import Linear (V2(..), V3(..))
import Hickory.Types (Size(..), Rect (Rect))

data RelativeRect a b = RRect (RelativeVec a b) (RelativeVec a b)

data RelativeVec a b = RVec (RelativeScalar a b) (RelativeScalar a b)

data RelativeScalar fract offset = RScal fract offset

transform :: (Fractional a, Real b, Real c) => RelativeScalar a b -> c -> a
transform (RScal fract offset) val = fract * realToFrac val + realToFrac offset

beg :: Num b => a -> RelativeScalar b a
beg = RScal 0
end :: (Num b, Num a) => a -> RelativeScalar b a
end a = RScal 1 (negate a)
center :: Fractional b => a -> RelativeScalar b a
center = RScal 0.5

addConst :: Num a => RelativeScalar b a -> a -> RelativeScalar b a
addConst (RScal fr con) plusConst = RScal fr (con + plusConst)

screenPos :: (Real a) => Size a -> RelativeVec Scalar Scalar -> V3 Scalar
screenPos (Size w h) (RVec xl yl) = V3 (transform xl w) (transform yl h) 0

transform2 :: (Real a) => Size a -> RelativeVec Scalar Scalar -> V2 Scalar
transform2 (Size w h) (RVec xl yl) = V2 (transform xl w) (transform yl h)

transformRect :: (Real b, Real a) => RelativeRect Scalar b -> Size a -> Rect
transformRect (RRect (RVec rx ry) (RVec rw rh)) (Size w h) =
        Rect (V2 (transform rx w) (transform ry h)) (Size (transform rw w) (transform rh h))
