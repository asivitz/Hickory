{-# LANGUAGE FlexibleInstances #-}

module Hickory.Math.Interpolate where

import Hickory.Math.Vector
import Linear (V2,V3,V4, M44)
import Linear.Vector (lerp)

nlerp :: Num a => a -> a -> a -> a
nlerp fract a b = (a * (1 - fract)) + (b * fract)

class Interpolatable i where
  glerp :: Float -> i -> i -> i

instance Interpolatable Float where
  glerp = nlerp

{-instance (Num a, Additive f) => Interpolatable (f a) where-}
        {-glerp = lerp-}

instance (Num a, Fractional a) => Interpolatable (V2 a) where glerp fr = lerp (realToFrac fr)
instance (Num a, Fractional a) => Interpolatable (V3 a) where glerp fr = lerp (realToFrac fr)
instance (Num a, Fractional a) => Interpolatable (V4 a) where glerp fr = lerp (realToFrac fr)
instance (Num a, Fractional a) => Interpolatable (M44 a) where glerp fr = liftA2 (lerp (realToFrac fr))

instance (Interpolatable a, Interpolatable b) => Interpolatable (a, b)
  where glerp fr a b = (glerp fr (fst a) (fst b), glerp fr (snd a) (snd b))

instance ( Interpolatable a
         , Interpolatable b
         , Interpolatable c
         ) => Interpolatable (a, b, c)
  where glerp fr (a1, a2, a3) (b1, b2, b3) =
          ( glerp fr a1 b1
          , glerp fr a2 b2
          , glerp fr a3 b3
          )

instance ( Interpolatable a
         , Interpolatable b
         , Interpolatable c
         , Interpolatable d
         ) => Interpolatable (a, b, c, d)
  where glerp fr (a1, a2, a3, a4) (b1, b2, b3, b4) =
          ( glerp fr a1 b1
          , glerp fr a2 b2
          , glerp fr a3 b3
          , glerp fr a4 b4
          )
