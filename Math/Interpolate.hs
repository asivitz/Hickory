module Math.Interpolate where

import Math.Vector
import Math.Matrix

lerp :: Num a => a -> a -> a -> a
lerp fract a b = (a * (1 - fract)) + (b * fract)

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

instance Interpolatable Mat44 where
        glerp fract = mat44Lerp (realToFrac fract)
