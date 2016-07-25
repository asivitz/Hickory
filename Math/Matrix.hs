module Math.Matrix
    (
    module Linear.Matrix,
    module Linear.Projection,
    mkTranslation,
    mkRotation,
    Mat44,
    Mat44Raw,
    withMat44,
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
import Math.Vector
import Foreign.Marshal.Utils

type Mat44 = M44 Double
type Mat44Raw = (Ptr CFloat)

withMat44 :: Mat44 -> (Ptr CFloat -> IO b) -> IO b
withMat44 mat f = with (fmap (fmap realToFrac) $ transpose mat :: M44 CFloat) (f . castPtr)

mkRotation v ang = mkTransformation (axisAngle v ang) zero
mkTranslation v = identity & translation .~ v

mat44Lerp :: Double -> Mat44 -> Mat44 -> Mat44
mat44Lerp x = liftI2 (lerp x)
