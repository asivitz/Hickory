module Types.Types 
   ( 
   Size(..),
   FSize,
   nullSize,
   aspectRatio,
   screenPos,
   YScreenLoc(..),
   XScreenLoc(..),
   fracSize,
   viewportFromSize,
   Rect(..)
   ) where

import Math.Vector

data Size a = Size a a deriving (Show)

type FSize = Size Float

nullSize :: Num a => Size a
nullSize = (Size 0 0)

aspectRatio :: (Real a, Fractional b) => Size a -> b
aspectRatio (Size w h) = w' / h'
    where (Size w' h') = (Size (realToFrac w) (realToFrac h))

viewportFromSize :: Integral a => Size a -> V4
viewportFromSize (Size w h) = v4 0 0 (fromIntegral w) (fromIntegral h)

fracSize :: (Real a, Fractional b) => Size a -> Size b
fracSize (Size w h) = Size (realToFrac w) (realToFrac h)

data Rect = Rect V2 (Size Scalar)

data YScreenLoc a = STop a
                  | SBottom a
                  | SMiddle

data XScreenLoc a = SLeft a
                  | SRight a
                  | SCenter

xScreenPos :: (Real a, Fractional b) => a -> XScreenLoc a -> b
xScreenPos w (SLeft l) = realToFrac l
xScreenPos w (SRight r) = realToFrac (w - r)
xScreenPos w SCenter = (realToFrac w)/2

yScreenPos :: (Real a, Fractional b) => a -> YScreenLoc a -> b
yScreenPos h (STop t) = realToFrac (h - t)
yScreenPos h (SBottom b) = realToFrac b
yScreenPos h SMiddle = (realToFrac h)/2

screenPos :: Real a => Size a -> YScreenLoc a -> XScreenLoc a -> V3
screenPos (Size w h) yl xl = v3 (xScreenPos w xl) (yScreenPos h yl) 0
