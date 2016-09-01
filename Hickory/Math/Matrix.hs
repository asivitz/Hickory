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

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.ForeignPtr
import System.IO.Unsafe
import Linear.Vector
import Linear.V3
import Linear.Matrix
import Linear.Projection
import Linear.Quaternion
import Control.Lens
import Hickory.Math.Vector
import Foreign.Marshal.Utils

type Mat44 = M44 Double

mkRotation v ang = mkTransformation (axisAngle v ang) zero
mkTranslation v = identity & translation .~ v

mat44Lerp :: Double -> Mat44 -> Mat44 -> Mat44
mat44Lerp x = liftI2 (lerp x)
