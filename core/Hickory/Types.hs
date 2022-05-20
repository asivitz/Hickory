{-# LANGUAGE DeriveFunctor #-}

module Hickory.Types
   (
   Size(..),
   nullSize,
   mkSize,
   v2ToSize,
   aspectRatio,
   fracSize,
   viewportFromSize,
   Rect(..),
   posInRect,
   convertSize,
   screenCenter,
   relativePosInRect,
   rectExtents,
   rectFromExtents
   ) where

import Hickory.Math.Vector
import Linear (V2(..), V4(..), (^*))

data Size a = Size
  { width  :: a
  , height :: a
  } deriving (Show, Read, Functor)

data Rect = Rect
  { center :: V2 Scalar
  , size   :: Size Scalar
  } deriving (Show, Read)

instance Num a => Num (Size a) where
  Size w h + Size w' h' = Size (w + w') (h + h')
  Size w h - Size w' h' = Size (w - w') (h - h')
  Size w h * Size w' h' = Size (w * w') (h * h')
  abs (Size w h) = Size (abs w) (abs h)
  fromInteger i = Size (fromInteger i) (fromInteger i)
  signum (Size a b) = Size (signum a) (signum b)

convertSize :: (Real a, Fractional b) => Size a -> Size b
convertSize (Size a b) = Size (realToFrac a) (realToFrac b)

screenCenter :: Real a => Size a -> V2 Scalar
screenCenter (Size w h) = V2 (realToFrac w / 2) (realToFrac h / 2)

nullSize :: Num a => Size a
nullSize = Size 0 0

mkSize :: a -> Size a
mkSize a = Size a a

v2ToSize :: V2 a -> Size a
v2ToSize (V2 x y) = Size x y

aspectRatio :: (Real a, Fractional b) => Size a -> b
aspectRatio (Size w h) = w' / h'
    where (Size w' h') = Size (realToFrac w) (realToFrac h)

viewportFromSize :: Integral a => Size a -> V4 a
viewportFromSize (Size w h) = V4 0 0 (fromIntegral w) (fromIntegral h)

fracSize :: (Real a, Fractional b) => Size a -> Size b
fracSize (Size w h) = Size (realToFrac w) (realToFrac h)


posInRect :: V2 Scalar -> Rect -> Bool
posInRect (V2 px py) (Rect (V2 ox oy) (Size w h)) =
        (abs (ox - px) < (w/2)) && (abs (oy - py) < (h/2))

relativePosInRect :: V2 Scalar -> Rect -> Maybe (V2 Scalar)
relativePosInRect (V2 px py) (Rect (V2 ox oy) (Size w h)) =
        let rx = (px - ox + (w/2)) / w
            ry = (py - oy + (h/2)) / h
            in if (rx >= 0 && rx <= 1) && (ry >= 0 && ry <= 1)
                   then Just $ V2 rx ry
                   else Nothing

rectExtents :: Rect -> (V2 Scalar, V2 Scalar)
rectExtents (Rect cen (Size w h)) = (cen - offset, cen + offset)
  where offset = V2 (w/2) (h/2)

rectFromExtents :: (V2 Scalar, V2 Scalar) -> Rect
rectFromExtents (ll, ur) = Rect cen (Size (abs w) (abs h))
  where siz@(V2 w h) = ur - ll
        cen = ll + siz ^* 0.5
