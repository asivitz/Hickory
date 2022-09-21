{-# LANGUAGE NamedFieldPuns #-}

module Hickory.Camera where

import Hickory.Math.Vector
import Hickory.Math.Matrix
import Linear (V3, lerp)

data Projection = Perspective
  { fov :: Scalar
  , nearPlane :: Scalar
  , farPlane :: Scalar
  }
  | Ortho
  { width :: Scalar
  , near :: Scalar
  , far :: Scalar
  , shouldCenter :: Bool
  } deriving (Show)

shotMatrix :: Projection -> Scalar -> Mat44
shotMatrix Perspective { fov, nearPlane, farPlane } screenRatio =
  perspectiveProjection (realToFrac screenRatio) fov nearPlane farPlane
shotMatrix Ortho { width, near, far, shouldCenter } screenRatio = if shouldCenter
  -- As per Vulkan convention, the top of the screen toward -Y
  then orthographicProjection (-(width / 2)) (width / 2) (height / 2) (-(height / 2)) near far
  else orthographicProjection 0 width height 0 near far
  where height = width / realToFrac screenRatio

-- Drivers

data Route = Route (V3 Scalar) (Maybe Target) deriving Show

data Target = Target
  { tpos :: V3 Scalar
  , moveTime :: Scalar
  , moveDuration :: Scalar
  } deriving Show

cameraCenter :: Route -> V3 Scalar
cameraCenter (Route pos Nothing ) = pos
cameraCenter (Route pos (Just (Target tarpos time duration))) = lerp (realToFrac (time / duration)) tarpos pos

checkTarget :: Route -> Scalar -> Route
checkTarget r@(Route _pos Nothing) _ = r
checkTarget (Route pos (Just (Target tpos moveTime moveDuration))) delta =
  let time' = (moveTime + delta)
  in  if time' > moveDuration then Route tpos Nothing else Route pos (Just (Target tpos time' moveDuration))
