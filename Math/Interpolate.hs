module Math.Interpolate where

import Math.Vector
import Math.Matrix

nlerp :: Num a => a -> a -> a -> a
nlerp fract a b = (a * (1 - fract)) + (b * fract)

class Interpolatable i where
        glerp :: Scalar -> i -> i -> i

instance Interpolatable Double where
        glerp = nlerp
{-instance (Num a, Additive f) => Interpolatable (f a) where-}
        {-glerp = lerp-}
{-instance Interpolatable Vector3 where-}
        {-glerp = lerp-}
{-instance Interpolatable V4 where-}
        {-glerp = lerp-}

{-instance Interpolatable Mat44 where-}
        {-glerp fract = mat44Lerp (realToFrac fract)-}
