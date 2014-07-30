module Types.Types 
   ( 
   Size(..),
   FSize,
   nullSize
   ) where

data Size a = Size a a deriving (Show)

type FSize = Size Float

nullSize :: Num a => Size a
nullSize = (Size 0 0)
