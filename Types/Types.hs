module Types.Types 
   ( 
   Size(..),
   FSize,
   nullSize,
   aspectRatio
   ) where

data Size a = Size a a deriving (Show)

type FSize = Size Float

nullSize :: Num a => Size a
nullSize = (Size 0 0)

aspectRatio :: (Real a, Fractional b) => Size a -> b
aspectRatio (Size w h) = w' / h'
    where (Size w' h') = (Size (realToFrac w) (realToFrac h))
