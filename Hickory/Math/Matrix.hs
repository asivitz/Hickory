module Hickory.Math.Matrix
    (
    module Linear.Matrix,
    module Linear.Projection,
    mkTranslation,
    mkRotation,
    Mat44,
    mat44Lerp
    )
    where

import Linear.Vector
import Linear.Matrix
import Linear.Projection
import Linear.Quaternion
import Control.Lens

type Mat44 = M44 Double

mkRotation v ang = mkTransformation (axisAngle v ang) zero
mkTranslation v = identity & translation .~ v

mat44Lerp :: Double -> Mat44 -> Mat44 -> Mat44
mat44Lerp x = liftI2 (lerp x)
