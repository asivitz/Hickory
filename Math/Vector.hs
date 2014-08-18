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
    pZero,
    v2tov3,
    v3tov4,
    v4tov3,
    v3tov2
    )
    where


import Data.Vector.V2
import Data.Vector.V3
import Data.Vector.V4
import Data.Vector.Class

type V2 = Vector2
type V3 = Vector3
type V4 = Vector4

v2 = Vector2
v3 = Vector3
v4 = Vector4

pZero :: BasicVector a => a
pZero = vpromote 0

v2tov3 :: V2 -> Scalar -> V3
v2tov3 (Vector2 x y) z = v3 x y z

v3tov4 :: V3 -> Scalar -> V4
v3tov4 (Vector3 x y z) w = v4 x y z w

v3tov2 :: V3 -> V2
v3tov2 (Vector3 x y z) = v2 x y

v4tov3 :: V4 -> V3
v4tov3 (Vector4 x y z w) = (Vector3 x y z)

