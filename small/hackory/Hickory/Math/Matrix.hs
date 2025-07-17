module Hickory.Math.Matrix
  ( mkRotation
  , Mat44
  , Mat33
  , mat44Lerp
  , m44FromList
  , sizePosMat
  , size3PosMat
  , sizePosRotMat
  , size3PosRotMat
  , viewDirection
  , viewDirectionAndInverse
  , viewTarget
  , orthographicProjection
  , perspectiveProjection
  , prettyPrint
  , MakeMat44(..)
  , transformV3
  ) where

import Linear.Vector
import Linear.Matrix
import Linear.Quaternion
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Epsilon
import Linear.Metric
import Control.Lens
import Hickory.Types
import Hickory.Math.Vector
import Text.Printf (printf)

type Mat44 = M44 Scalar
type Mat33 = M33 Scalar

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

mat44Lerp :: Scalar -> Mat44 -> Mat44 -> Mat44
mat44Lerp x = liftI2 (lerp x)

sizePosMat :: Size Scalar -> V3 Scalar -> Mat44
sizePosMat (Size w h) pos = mkTranslation pos !*! scaled (V4 w h 1 1)

size3PosMat :: V3 Scalar -> V3 Scalar -> Mat44
size3PosMat (V3 w h d) pos = mkTranslation pos !*! scaled (V4 w h d 1)

sizePosRotMat :: Size Scalar -> V3 Scalar -> Scalar -> Mat44
sizePosRotMat (Size w h) pos rot = mkTranslation pos !*! mkRotation (V3 0 0 1) rot !*! scaled (V4 w h 1 1)

size3PosRotMat :: V3 Scalar -> V3 Scalar -> Scalar -> Mat44
size3PosRotMat (V3 w h d) pos rot = mkTranslation pos !*! mkRotation (V3 0 0 1) rot !*! scaled (V4 w h d 1)

-- |Row major, following the convention of the `linear` library
-- | a1 a2 a3 a4
-- | b1 b2 b3 b4
-- | c1 c2 c3 c4
-- | d1 d2 d3 d4
m44FromList :: [a] -> M44 a
m44FromList
  [a1,a2,a3,a4
  ,b1,b2,b3,b4
  ,c1,c2,c3,c4
  ,d1,d2,d3,d4] =
  V4 (V4 a1 a2 a3 a4)
     (V4 b1 b2 b3 b4)
     (V4 c1 c2 c3 c4)
     (V4 d1 d2 d3 d4)
m44FromList _ = error "Can't build matrix. Wrong size list."

-- |Orthographic projection following Vulkan conventions
-- (right handed coordinate system, depth dimension along +z, bottom of screen is toward +y)
orthographicProjection :: Fractional a => a -> a -> a -> a -> a -> a -> M44 a
orthographicProjection l r b t n f = V4
  (V4 (2 / (r - l)) 0             0             (- (r + l) / (r - l)))
  (V4 0             (2 / (b - t)) 0             (- (b + t) / (b - t)))
  (V4 0             0             (1 / (f - n)) (- n / (f - n)))
  (V4 0             0             0             1)

-- |Perspective projection following Vulkan conventions
-- (right handed coordinate system, depth dimension along +z, bottom of screen is toward +y)
perspectiveProjection :: Floating a => a -> a -> a -> a -> M44 a
perspectiveProjection screenRatio vfov n f = V4
  (V4 (1 / (screenRatio * tan (vfov/2))) 0                 0             0)
  (V4 0                                 (1 / tan (vfov/2)) 0             0)
  (V4 0                                 0                 (f / (f - n)) (- f * n / (f - n)))
  (V4 0                                 0                 1             0)

-- |Construct a view matrix from a camera position, direction, and up vector
viewDirection :: (Floating a, Epsilon a) => V3 a -> V3 a -> V3 a -> M44 a
viewDirection position direction up = fst $ viewDirectionAndInverse position direction up

-- |Construct a view matrix from a camera position, direction, and up vector
viewDirectionAndInverse :: (Floating a, Epsilon a) => V3 a -> V3 a -> V3 a -> (M44 a, M44 a)
viewDirectionAndInverse position direction up = (forward, backward)
  where
  w = normalize direction
  u = normalize (cross w up)
  v = cross w u
  basis' = V3 u v w
  forward = mkTransformationMat basis' zero -- Rotate to align w/ camera's ortho normal basis
        !*! mkTranslation (-position) -- Move from world space to camera's center
  backward = mkTranslation position -- Move from world space to camera's center
         !*! mkTransformationMat (transpose basis') zero -- Rotate to align w/ camera's ortho normal basis

viewTarget :: (Floating a, Epsilon a) => V3 a -> V3 a -> V3 a -> M44 a
viewTarget position target = viewDirection position (target - position)

prettyPrint :: M44 Float -> IO ()
prettyPrint (V4
  (V4 a1 a2 a3 a4)
  (V4 b1 b2 b3 b4)
  (V4 c1 c2 c3 c4)
  (V4 d1 d2 d3 d4)
  )
  = do
    printf "[%.2f %.2f %.2f %.2f]\n" a1 a2 a3 a4
    printf "[%.2f %.2f %.2f %.2f]\n" b1 b2 b3 b4
    printf "[%.2f %.2f %.2f %.2f]\n" c1 c2 c3 c4
    printf "[%.2f %.2f %.2f %.2f]\n" d1 d2 d3 d4

transformV3 :: (Num a) => M44 a -> V3 a -> V3 a
transformV3 m v = (m !* v3tov4 v 1) ^. _xyz
