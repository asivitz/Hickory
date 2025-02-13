{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

module Hickory.Math.Vector
  ( v2tov3
  , v3tov4
  , movePos
  , movePosNoStop
  , moveVal
  , vmidpoint
  , vnull
  , vabsangle
  , v2angle
  , v2clockwise
  , v2rotate
  , v2SegmentsIntersect
  , v2perp
  , vunpackFractional
  , timeToIntersection
  , intersectionPoint
  , Scalar
  , v4FromList
  , v3FromList
  , v3map
  , v2map
  , clamp
  , rlerp
  , rlerpClamp
  , angleMinusClockwise
  , angleMinus
  , v2ortho
  ) where

import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector
import Linear.Epsilon
import Linear.Metric
import Data.Foldable (toList)
import GHC.Records (HasField(..))
import Data.Generics.Product (field, getField, position, setField)
import Data.Generics.Sum (_Ctor)
import Data.Generics.Labels

type Vector v a = (Metric v, Epsilon (v a), Additive v, Floating a, Real a, RealFloat a)

type Scalar = Float

v2tov3 :: V2 a -> a -> V3 a
v2tov3 (V2 x y) = V3 x y

v3tov4 :: V3 a -> a -> V4 a
v3tov4 (V3 x y z) = V4 x y z

vmidpoint :: Vector f a => f a -> f a -> f a
vmidpoint = lerp 0.5

vnull :: Vector f a => f a -> Bool
vnull = nearZero

vabsangle :: Vector f a => f a -> f a -> a
-- vabsangle a b = acos $ vdot (vnormalise a) (vnormalise b)
-- vabsangle a b = acos $ clamp (dot a b / (norm a * norm b)) (-1) 1
-- This version should be the most accurate
vabsangle a b = 2 * atan (norm ((a ^* lenb) - (b ^* lena)) / norm (a ^* lenb + b ^* lena))
  where
  lena = norm a
  lenb = norm b

v2angle :: (Epsilon a, RealFloat a) => V2 a -> V2 a -> a
v2angle a b = if v2clockwise a b then -ang else ang
        where ang = vabsangle a b

v2clockwise :: (Num a, Ord a) => V2 a -> V2 a -> Bool
v2clockwise (V2 xa ya) (V2 xb yb) = xa * yb - ya * xb <= 0

v2SegmentsIntersect :: (Num a, Ord a) => (V2 a, V2 a) -> (V2 a, V2 a) -> Bool
v2SegmentsIntersect (a, a') (b, b') = v2clockwise (b - a) adiff /= v2clockwise (b' - a) adiff && v2clockwise (a - b) bdiff /= v2clockwise (a' - b) bdiff
    where adiff = a' - a
          bdiff = b' - b

vunpackFractional :: (Real a, Fractional b, Foldable f) => f a -> [b]
vunpackFractional = map realToFrac . toList

movePos :: Vector f a => f a -> f a -> a -> a -> f a
movePos p1 p2 speed time =
        let diff = p2 - p1
            amt = speed * time
            mag = norm diff in
                if mag > amt
                    then p1 + diff ^* (amt / mag)
                    else p2

movePosNoStop :: Vector f a => f a -> f a -> a -> a -> f a
movePosNoStop p1 p2 speed time =
        let diff = p2 - p1
            amt = speed * time
            mag = norm diff in
                p1 + diff ^* (amt / mag)

moveVal :: (Real a, Fractional a) => a -> a -> a -> a -> a
moveVal p1 p2 speed time =
        let diff = p2 - p1
            amt = speed * time
            mag = abs diff in
                if mag > amt
                    then p1 + diff * (amt / mag)
                    else p2

-- Extra

timeToIntersection :: (Vector f a) => a -> f a -> f a -> Maybe a
timeToIntersection speed pos vel =
        let a = dot pos pos
            b = 2 * dot pos vel
            c = dot vel vel - speed * speed
            desc = b * b - 4 * a * c
            in if desc >= 0
                      then let p1 = (-1) * b
                               p2 = sqrt desc
                               p3 = 2 * a
                               u1 = (p1 + p2) / p3
                               u2 = (p1 - p2) / p3
                               t1 = 1 / u1
                               t2 = 1 / u2
                               vals = filter (\x -> x > 0 && (not . isInfinite) x) [t1, t2]
                               in if null vals then Nothing else Just $ minimum vals
                      else Nothing

intersectionPoint :: Vector f a => f a -> a -> f a -> f a -> Maybe (f a)
intersectionPoint p1 speed p2 vel = do
  t <- timeToIntersection speed (p2 - p1) vel
  pure $ p2 + (vel ^* t)

v2rotate :: Floating a => V2 a -> a -> V2 a
v2rotate (V2 x y) ang = V2 (x * co - y * si) (y * co + x * si)
    where co = cos ang
          si = sin ang

v4FromList :: [a] -> V4 a
v4FromList [a,b,c,d] = V4 a b c d
v4FromList _ = error "Can't build vector. Wrong size list."

v3FromList :: [a] -> V3 a
v3FromList [a,b,c] = V3 a b c
v3FromList _ = error "Can't build vector. Wrong size list."

v2perp :: Num a => V2 a -> V2 a
v2perp (V2 x y) = V2 y (negate x)

v3map :: (a -> b) -> V3 a -> V3 b
v3map f (V3 a b c) = V3 (f a) (f b) (f c)

v2map :: (a -> b) -> V2 a -> V2 b
v2map f (V2 a b) = V2 (f a) (f b)

clamp :: Ord a => a -> a -> a -> a
clamp a low high = min (max a low) high

rlerp :: Fractional a => a -> a -> a -> a
rlerp a low high = (a - low) / (high - low)

rlerpClamp :: (Fractional a, Ord a) => a -> a -> a -> a
rlerpClamp a low high = rlerp (clamp a low high) low high

-- Difference in angle, given a is clockwise from b
angleMinusClockwise :: (Ord a, Floating a) => a -> a -> a
angleMinusClockwise a b = negate $ (if a > b then a - 2 * pi else a) - b

-- Difference in angle, given a is clockwise from b
angleMinus :: (Ord a, Floating a) => a -> a -> a
angleMinus a b = (if a < b then a + 2 * pi else a) - b

v2ortho :: Num a => V2 a -> V2 a
v2ortho (V2 x y) = V2 y (-x)

instance HasField "x" (V2 a) a where getField (V2 x y) = x
instance HasField "y" (V2 a) a where getField (V2 x y) = y
instance HasField "x" (V3 a) a where getField (V3 x y z) = x
instance HasField "y" (V3 a) a where getField (V3 x y z) = y
instance HasField "z" (V3 a) a where getField (V3 x y z) = z
instance HasField "xy" (V3 a) (V2 a) where getField (V3 x y z) = V2 x y
instance HasField "x" (V4 a) a where getField (V4 x y z w) = x
instance HasField "y" (V4 a) a where getField (V4 x y z w) = y
instance HasField "z" (V4 a) a where getField (V4 x y z w) = z
instance HasField "w" (V4 a) a where getField (V4 x y z w) = w
instance HasField "xy" (V4 a) (V2 a) where getField (V4 x y z w) = V2 x y
instance HasField "xyz" (V4 a) (V3 a) where getField (V4 x y z w) = V3 x y z
