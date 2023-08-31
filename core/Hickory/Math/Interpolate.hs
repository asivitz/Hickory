{-# LANGUAGE FlexibleInstances #-}

module Hickory.Math.Interpolate where

import Hickory.Math.Vector
import Linear (V2,V3,V4, M44)
import Linear.Vector (lerp)
import Control.Applicative (liftA2)

nlerp :: Num a => a -> a -> a -> a
nlerp fract a b = (a * (1 - fract)) + (b * fract)

class Interpolatable i where
  glerp :: Scalar -> i -> i -> i

instance Interpolatable Scalar where
  glerp = nlerp

{-instance (Num a, Additive f) => Interpolatable (f a) where-}
        {-glerp = lerp-}

instance Interpolatable (V2 Scalar) where glerp fr = flip (lerp fr)
instance Interpolatable (V3 Scalar) where glerp fr = flip (lerp fr)
instance Interpolatable (V4 Scalar) where glerp fr = flip (lerp fr)
instance Interpolatable (M44 Scalar) where glerp fr a b = liftA2 (lerp fr) b a

instance (Interpolatable a, Interpolatable b) => Interpolatable (a, b)
  where glerp fr a b = (glerp fr (fst a) (fst b), glerp fr (snd a) (snd b))

{-instance Interpolatable V4 where-}
        {-glerp = lerp-}

{-instance Interpolatable Mat44 where-}
        {-glerp fract = mat44Lerp (realToFrac fract)-}
