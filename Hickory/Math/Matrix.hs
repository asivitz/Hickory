module Hickory.Math.Matrix
    (
    module Linear.Matrix,
    module Linear.Projection,
    mkScale,
    mkTranslation,
    mkRotation,
    Mat44,
    Mat33,
    mat44Lerp,
    mat44FromList
    )
    where

import Linear.Vector
import Linear.Matrix
import Linear.Projection
import Linear.Quaternion
import Linear.V3
import Linear.V4
import Control.Lens

type Mat44 = M44 Double
type Mat33 = M33 Double

mkRotation v ang = mkTransformation (axisAngle v ang) zero
mkTranslation v = identity & translation .~ v
mkScale (V3 x y z) = scaled (V4 x y z 1)

mat44Lerp :: Double -> Mat44 -> Mat44 -> Mat44
mat44Lerp x = liftI2 (lerp x)

mat44FromList :: [a] -> M44 a
mat44FromList [a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4] =
        transpose $ V4 (V4 a1 a2 a3 a4) (V4 b1 b2 b3 b4) (V4 c1 c2 c3 c4) (V4 d1 d2 d3 d4)
mat44FromList _ = error "Can't build matrix. Wrong size list."
