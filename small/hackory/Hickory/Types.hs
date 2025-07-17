{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}

module Hickory.Types
   (
   Size(..),
   nullSize,
   mkSize,
   v2ToSize,
   aspectRatio,
   fracSize,
   viewportFromSize,
   convertSize,
   sizeToV2,
   ) where

import Linear (V2(..), V4(..), (^*))
import GHC.Generics (Generic)

data Size a = Size
  { width  :: a
  , height :: a
  } deriving (Show, Read, Functor, Generic)

instance Num a => Num (Size a) where
  Size w h + Size w' h' = Size (w + w') (h + h')
  Size w h - Size w' h' = Size (w - w') (h - h')
  Size w h * Size w' h' = Size (w * w') (h * h')
  abs (Size w h) = Size (abs w) (abs h)
  fromInteger i = Size (fromInteger i) (fromInteger i)
  signum (Size a b) = Size (signum a) (signum b)

instance Fractional a => Fractional (Size a) where
  fromRational rat = Size (fromRational rat) (fromRational rat)
  (Size a1 a2) / (Size b1 b2) = Size (a1 / b1) (a2 / b2)

convertSize :: (Real a, Fractional b) => Size a -> Size b
convertSize (Size a b) = Size (realToFrac a) (realToFrac b)

nullSize :: Num a => Size a
nullSize = Size 0 0

mkSize :: a -> Size a
mkSize a = Size a a

v2ToSize :: V2 a -> Size a
v2ToSize (V2 x y) = Size x y

sizeToV2 :: Size a -> V2 a
sizeToV2 (Size x y) = V2 x y

aspectRatio :: (Real a, Fractional b) => Size a -> b
aspectRatio (Size w h) = w' / h'
    where (Size w' h') = Size (realToFrac w) (realToFrac h)

viewportFromSize :: Integral a => Size a -> V4 a
viewportFromSize (Size w h) = V4 0 0 (fromIntegral w) (fromIntegral h)

fracSize :: (Real a, Fractional b) => Size a -> Size b
fracSize (Size w h) = Size (realToFrac w) (realToFrac h)
