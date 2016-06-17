{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Math.Vector 
    (
    module Data.Vector.V2,
    module Data.Vector.V3,
    module Data.Vector.V4,
    module Data.Vector.Class,
    V2,
    V3,
    V4,
    v2,
    v3,
    v4,
    vZero,
    v2tov3,
    v3tov4,
    v4tov3,
    v3tov2,
    movePos,
    movePosNoStop,
    moveVal,
    vmidpoint,
    vnull,
    vabsangle,
    v2angle,
    v2clockwise,
    v2SegmentsIntersect,
    vunpackFractional,
    Interpolatable,
    glerp
    )
    where


import Data.Vector.V2
import Data.Vector.V3
import Data.Vector.V4
import Data.Vector.Class
import Utils.Utils
import GHC.Generics
import Data.Serialize
import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty
import Text.Printf

type V2 = Vector2
type V3 = Vector3
type V4 = Vector4

v2 = Vector2
v3 = Vector3
v4 = Vector4

deriving instance Generic Vector2
instance Serialize Vector2
instance Out Vector2 where
  doc (Vector2 x y) =  parens $ text (printf "%.2f,%.2f" x y)
  docPrec _ = doc

deriving instance Generic Vector3
instance Serialize Vector3
instance Out Vector3 where
  doc (Vector3 x y z) =  parens $ text (printf "%.2f,%.2f,%.2f" x y z)
  docPrec _ = doc

deriving instance Generic Vector4
instance Serialize Vector4
instance Out Vector4 where
  doc (Vector4 x y z w) =  parens $ text (printf "%.2f,%.2f,%.2f,%.2f" x y z w)
  docPrec _ = doc

vZero :: BasicVector a => a
vZero = vpromote 0

v2tov3 :: V2 -> Scalar -> V3
v2tov3 (Vector2 x y) z = v3 x y z

v3tov4 :: V3 -> Scalar -> V4
v3tov4 (Vector3 x y z) w = v4 x y z w

v3tov2 :: V3 -> V2
v3tov2 (Vector3 x y z) = v2 x y

v4tov3 :: V4 -> V3
v4tov3 (Vector4 x y z w) = (Vector3 x y z)

vmidpoint :: Vector a => a -> a -> a
vmidpoint v t = vlinear 0.5 v t

vnull :: Vector a => a -> Bool
vnull v = vmag v == 0

vabsangle :: Vector a => a -> a -> Scalar
-- TODO: Performance test the difference 
-- vangle a b = acos $ vdot (vnormalise a) (vnormalise b)
vabsangle a b = acos $ vdot a b / (vmag a * vmag b)

v2angle :: Vector2 -> Vector2 -> Scalar
v2angle a b = if v2clockwise a b then ang else -ang
        where ang = vabsangle a b

v2clockwise :: Vector2 -> Vector2 -> Bool
v2clockwise a b = v2x a * v2y b - v2y a * v2x b >= 0

v2SegmentsIntersect :: (V2, V2) -> (V2, V2) -> Bool
v2SegmentsIntersect (a, a') (b, b') = v2clockwise (b - a) adiff /= v2clockwise (b' - a) adiff && v2clockwise (a - b) bdiff /= v2clockwise (a' - b) bdiff
    where adiff = a' - a
          bdiff = b' - b

vunpackFractional :: (Fractional a, Vector v) => v -> [a]
vunpackFractional v = map realToFrac (vunpack v)

movePos :: Vector a => a -> a -> Scalar -> Scalar -> a
movePos p1 p2 speed time = 
        let diff = p2 - p1 
            amt = speed * time 
            mag = vmag diff in
                if mag > amt
                    then p1 + diff |* (amt / mag)
                    else p2

movePosNoStop :: Vector a => a -> a -> Scalar -> Scalar -> a
movePosNoStop p1 p2 speed time = 
        let diff = p2 - p1 
            amt = speed * time 
            mag = vmag diff in
                p1 + diff |* (amt / mag)

moveVal :: (Real a, Fractional a) => a -> a -> a -> a -> a
moveVal p1 p2 speed time = 
        let diff = p2 - p1 
            amt = speed * time 
            mag = abs diff in
                if mag > amt
                    then p1 + diff * (amt / mag)
                    else p2

class Interpolatable i where
        glerp :: Scalar -> i -> i -> i
        
instance Interpolatable Double where
        glerp = lerp
instance Interpolatable Vector2 where
        glerp = vlinear
instance Interpolatable Vector3 where
        glerp = vlinear
instance Interpolatable Vector4 where
        glerp = vlinear
