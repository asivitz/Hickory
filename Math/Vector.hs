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
    mat44TranslateV,
    withVec4,
    v2tov3
    )
    where


import Data.Vector.V2
import Data.Vector.V3
import Data.Vector.V4
import Data.Vector.Class
import Math.Matrix
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Array

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

mat44TranslateV :: V3 -> Mat44 -> Mat44
mat44TranslateV (Vector3 x y z) m = 
    mat44Translate (realToFrac x) (realToFrac y) (realToFrac z) $ m

withVec4 :: Vector4 -> (Ptr CFloat -> IO b) -> IO b
withVec4 v func = let lst = map realToFrac $ vunpack v in
    withArray lst func

