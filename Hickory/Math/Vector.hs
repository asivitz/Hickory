{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hickory.Math.Vector
    (
    module Linear.V2,
    module Linear.V3,
    module Linear.V4,
    module Linear.Vector,
    v2,
    v3,
    v4,
    v2tov3,
    v3tov4,
    movePos,
    movePosNoStop,
    moveVal,
    vmidpoint,
    vnull,
    vabsangle,
    v2angle,
    v2clockwise,
    v2rotate,
    v2SegmentsIntersect,
    v2perp,
    vunpackFractional,
    timeToIntersection,
    intersectionPoint,
    Scalar,
    v4FromList,
    v3FromList,
    v3map,
    v2map
    )
    where


import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector
import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty
import Text.Printf
import Linear.Epsilon
import Linear.Metric
import Data.Foldable (toList)

v2 = V2
v3 = V3
v4 = V4

type Vector v a = (Metric v, Epsilon (v a), Additive v, Floating a, Real a, RealFloat a)

instance (PrintfArg a, Out a) => Out (V2 a) where
  doc (V2 x y) =  parens $ text (printf "%.2f,%.2f" x y)
  docPrec _ = doc

instance (PrintfArg a, Out a) => Out (V3 a) where
  doc (V3 x y z) =  parens $ text (printf "%.2f,%.2f,%.2f" x y z)
  docPrec _ = doc

instance (PrintfArg a, Out a) => Out (V4 a) where
  doc (V4 x y z w) =  parens $ text (printf "%.2f,%.2f,%.2f,%.2f" x y z w)
  docPrec _ = doc

type Scalar = Double

v2tov3 :: V2 a -> a -> V3 a
v2tov3 (V2 x y) z = v3 x y z

v3tov4 :: V3 a -> a -> V4 a
v3tov4 (V3 x y z) w = v4 x y z w

vmidpoint :: Vector f a => f a -> f a -> f a
vmidpoint = lerp 0.5

vnull :: Vector f a => f a -> Bool
vnull = nearZero

vabsangle :: Vector f a => f a -> f a -> a
-- TODO: Performance test the difference
-- vangle a b = acos $ vdot (vnormalise a) (vnormalise b)
vabsangle a b = acos $ dot a b / (norm a * norm b)

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
intersectionPoint p1 speed p2 vel = maybe Nothing (\t -> Just $ p2 + (vel ^* t)) (timeToIntersection speed (p2 - p1) vel)

v2rotate :: Floating a => V2 a -> a -> V2 a
v2rotate (V2 x y) ang = v2 (x * co - y * si) (y * co + x * si)
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
