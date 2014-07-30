module Math.Vector where

data V2 a = V2 a a deriving (Show)
type Vec = V2 Float

pZero :: Num a => V2 a
pZero = (V2 0 0)

