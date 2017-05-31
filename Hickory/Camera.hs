{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module Hickory.Camera where

import Hickory.Math.Vector
import Hickory.Math.Matrix
import Text.PrettyPrint.GenericPretty

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
  } deriving (Show, Generic)

data Camera = Camera
  { _cameraProj   :: Projection
  , _cameraCenter :: V3 Scalar
  , _cameraTarget :: V3 Scalar
  , _cameraUp     :: V3 Scalar
  } deriving (Show, Generic)

instance Out Projection where
instance Out Camera where

shotMatrix :: Projection -> Scalar -> Mat44
shotMatrix Perspective { fov, nearPlane, farPlane } screenRatio =
        perspective fov (realToFrac screenRatio) nearPlane farPlane
shotMatrix Ortho { width, near, far, shouldCenter } screenRatio =
        if shouldCenter
            then
                ortho (-(width/2)) (width/2) (-(height/2)) (height/2) near far
            else
                ortho 0 width 0 height near far
        where height = width / (realToFrac screenRatio)

worldViewMatrix :: Projection -> Scalar -> V3 Scalar -> V3 Scalar -> V3 Scalar -> Mat44
worldViewMatrix proj screenRatio pos target up =
        let worldMatrix = shotMatrix proj screenRatio
            camera = lookAt pos target up in
                worldMatrix !*! camera

cameraMatrix :: Camera -> Scalar -> Mat44
cameraMatrix (Camera proj center target up) screenRatio = worldViewMatrix proj screenRatio center target up

-- Drivers

data Route = Route (V3 Scalar) (Maybe Target) deriving Show

data Target = Target {
            tpos :: (V3 Scalar),
            moveTime :: Double,
            moveDuration :: Double
            } deriving Show

cameraCenter :: Route -> (V3 Scalar)
cameraCenter (Route pos Nothing ) = pos
cameraCenter (Route pos (Just (Target tarpos time duration))) = lerp (realToFrac (time / duration)) tarpos pos

checkTarget :: Route -> Double -> Route
checkTarget r@(Route pos Nothing) _ = r
checkTarget (Route pos (Just (Target tpos moveTime moveDuration))) delta = let time' = (moveTime + delta) in
    if time' > moveDuration
        then (Route tpos Nothing)
        else (Route pos (Just (Target tpos time' moveDuration)))
