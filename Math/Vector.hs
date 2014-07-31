module Math.Vector where

data V2 a = V2 a a deriving (Eq)
type Vec = V2 Float

pZero :: Num a => V2 a
pZero = (V2 0 0)

instance Show a => Show (V2 a) where
  show (V2 x y) = "<" ++ (show x) ++ ", " ++ (show y) ++ ">"

mapV2 f (V2 x y) = (V2 (f x) (f y))
zipV2 f (V2 x y) (V2 x' y') = V2 (f x x') (f y y')

scaleV2 :: Num a => a -> V2 a -> V2 a
scaleV2 a = mapV2 (a*)

instance Num a => Num (V2 a) where
  (+) = zipV2 (+)
  (-) = zipV2 (-)
  (*) = zipV2 (*)
  abs = mapV2 abs
  signum = mapV2 signum
  negate = mapV2 negate
  fromInteger i = V2 (fromInteger i) (fromInteger i)

lerp :: Num a => a -> V2 a -> V2 a -> V2 a
lerp fraction a b = (scaleV2 (1 - fraction) a) + (scaleV2 fraction b)
