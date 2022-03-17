module Hickory.Math.Matrix
  ( mkScale
  , mkTranslation
  , mkRotation
  , Mat44
  , Mat33
  , mat44Lerp
  , mat44FromList
  , sizePosMat
  , size3PosMat
  , sizePosRotMat
  , size3PosRotMat
  ) where

import Linear.Vector
import Linear.Matrix
import Linear.Quaternion
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Epsilon
import Control.Lens
import Hickory.Types
import Hickory.Math.Vector

type Mat44 = M44 Double
type Mat33 = M33 Double

class MakeMat44 v where
  mkScale       :: Num a => v a -> M44 a
  mkTranslation :: Num a => v a -> M44 a

instance MakeMat44 V2 where
  mkScale (V2 x y) = scaled (V4 x y 1 1)
  mkTranslation (V2 x y) = mkTranslation3 (V3 x y 0)

instance MakeMat44 V3 where
  mkScale (V3 x y z) = scaled (V4 x y z 1)
  mkTranslation = mkTranslation3

mkRotation :: (Floating a, Linear.Epsilon.Epsilon a) => V3 a -> a -> M44 a
mkRotation v ang = mkTransformation (axisAngle v ang) zero

mkTranslation3 :: (Num a) => V3 a -> M44 a
mkTranslation3 v = identity & translation .~ v

mat44Lerp :: Double -> Mat44 -> Mat44 -> Mat44
mat44Lerp x = liftI2 (lerp x)

mat44FromList :: [a] -> M44 a
mat44FromList [a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4] =
        transpose $ V4 (V4 a1 a2 a3 a4) (V4 b1 b2 b3 b4) (V4 c1 c2 c3 c4) (V4 d1 d2 d3 d4)
mat44FromList _ = error "Can't build matrix. Wrong size list."

sizePosMat :: Size Scalar -> V3 Scalar -> Mat44
sizePosMat (Size w h) pos = mkTranslation pos !*! scaled (V4 w h 1 1)

size3PosMat :: V3 Scalar -> V3 Scalar -> Mat44
size3PosMat (V3 w h d) pos = mkTranslation pos !*! scaled (V4 w h d 1)

sizePosRotMat :: Size Scalar -> V3 Scalar -> Scalar -> Mat44
sizePosRotMat (Size w h) pos rot = mkTranslation pos !*! mkRotation (V3 0 0 1) rot !*! scaled (V4 w h 1 1)

size3PosRotMat :: V3 Scalar -> V3 Scalar -> Scalar -> Mat44
size3PosRotMat (V3 w h d) pos rot = mkTranslation pos !*! mkRotation (V3 0 0 1) rot !*! scaled (V4 w h d 1)
