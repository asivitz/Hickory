module Hickory.Editor.General where

import Linear (M44, column, V3 (..), V2 (..), V4 (..), norm, _x, _y, _z, Epsilon (..))
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Control.Lens ((^.), (&))
import Linear (axisAngle, identity, Quaternion (..), M44, translation, mkTransformationMat, fromQuaternion, m33_to_m44, unit, Epsilon(..), column, V3 (..), V2 (..), V4 (..), (!*!), normalize, (^*), _x, _y, _z, cross, norm, zero, (^/), _xyz)
import Control.Lens (traversed, (^.), (&), (%~), (.~), (^?), ix, (<&>), (?~), at, _Just, sumOf, _2)

-- Move this into better general use modules

matScale :: Floating a => M44 a -> V3 a
matScale m = V3 (norm $ m ^. column _x) (norm $ m ^. column _y) (norm $ m ^. column _z)

matEuler :: RealFloat a => M44 a -> V3 a
matEuler m =
  if sy < 1e-6
  then let
          x = atan2 (-m12) m11
          y = atan2 (-m20) sy
          z = 0
       in V3 x y z
  else let
          x = atan2 m21 m22
          y = atan2 (-m20) sy
          z = atan2 m10 m00
       in V3 x y z
  where
  V4 (V4 m00 _m01 _m02 _m03)
     (V4 m10 m11 m12 _m13)
     (V4 m20 m21 m22 _m23)
     (V4 _m30 _m31 _m32 _m33) = m
  sy = sqrt (m00 * m00 + m10 * m10)

setScale :: (Floating a, Epsilon a) => V3 a -> M44 a -> M44 a
setScale v m = m & column _x %~ (^* (v ^. _x)) . normalize . (\x -> if nearZero x then unit _x else x)
                 & column _y %~ (^* (v ^. _y)) . normalize . (\x -> if nearZero x then unit _y else x)
                 & column _z %~ (^* (v ^. _z)) . normalize . (\x -> if nearZero x then unit _z else x)
