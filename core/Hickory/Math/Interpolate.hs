{-# LANGUAGE FlexibleInstances #-}

module Hickory.Math.Interpolate where

import Hickory.Math.Vector
import Linear (V2,V3,V4)
import Linear.Vector (lerp)

nlerp :: Num a => a -> a -> a -> a
nlerp fract a b = (a * (1 - fract)) + (b * fract)

class Interpolatable i where
  glerp :: Scalar -> i -> i -> i

instance Interpolatable Double where
  glerp = nlerp

{-instance (Num a, Additive f) => Interpolatable (f a) where-}
        {-glerp = lerp-}

instance Interpolatable (V2 Double) where glerp fr = flip (lerp fr)
instance Interpolatable (V3 Double) where glerp fr = flip (lerp fr)
instance Interpolatable (V4 Double) where glerp fr = flip (lerp fr)

{-instance Interpolatable V4 where-}
        {-glerp = lerp-}

{-instance Interpolatable Mat44 where-}
        {-glerp fract = mat44Lerp (realToFrac fract)-}
