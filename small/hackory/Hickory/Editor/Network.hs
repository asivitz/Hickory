{-# LANGUAGE OverloadedLabels, OverloadedRecordDot, OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Hickory.Editor.Network where

import Hickory.Types (Size (..), aspectRatio)
import Data.Maybe (fromMaybe, isJust)
import Linear (axisAngle, identity, Quaternion (..), translation, mkTransformationMat, fromQuaternion, m33_to_m44, V3 (..), V2 (..), V4 (..), (!*!), normalize, (^*), cross, norm, zero, (^/), _xyz)
import qualified Data.HashMap.Strict as Map
import Control.Lens (traversed, (^.), (&), (%~), (<&>), sumOf)
import Data.HashMap.Strict (HashMap)
import Hickory.Editor.Types
import qualified Data.Vector.Storable as SV
import qualified Hickory.Vulkan.Types as H
import Safe (maximumMay, headMay)
import Data.Traversable (for)
import Control.Monad (join, mfilter, void, when)
import Data.IORef (newIORef, readIORef, writeIORef, atomicModifyIORef', IORef)
import Control.Applicative ((<|>), asum)
import Data.List.Extra (notNull)
import qualified Data.Enum.Set as ES
import Control.Monad.Writer.Strict (Writer)
import qualified Data.HashMap.Strict as HashMap
import Data.Word (Word32)

data Editor = Editor
  { selectedObjectIDs :: [Word32]
  }


-- https://en.wikipedia.org/wiki/Conversion_between_quaternions_and_Euler_angles#Quaternion_to_Euler_angles_conversion
-- https://creativecommons.org/licenses/by-sa/3.0/
quaternionToEuler :: RealFloat a => Quaternion a -> V3 a
quaternionToEuler (Quaternion w (V3 x y z)) = V3 roll pitch yaw
  where
  -- roll (x-axis rotation)
  sinr_cosp = 2 * (w * x + y * z)
  cosr_cosp = 1 - 2 * (x * x + y * y)
  roll = atan2 sinr_cosp cosr_cosp

  -- pitch (y-axis rotation)
  sinp = 2 * (w * y - z * x)
  pitch =
    if abs sinp  >= 1
    then pi/2 * signum sinp -- use 90 degrees if out of range
    else asin sinp

  -- yaw (z-axis rotation)
  siny_cosp = 2 * (w * z + x * y)
  cosy_cosp = 1 - 2 * (y * y + z * z)
  yaw = atan2 siny_cosp cosy_cosp
