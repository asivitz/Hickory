{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module Camera.Camera where

import Math.Vector
import Math.Matrix
import Math.VectorMatrix
import Text.PrettyPrint.GenericPretty

data Projection = Perspective {
                     fov :: Scalar,
                     nearPlane :: Scalar,
                     farPlane :: Scalar }
                  | Ortho {
                     width :: Scalar,
                     near :: Scalar,
                     far :: Scalar,
                     shouldCenter :: Bool }
                     deriving (Show, Generic)
data Camera = Camera Projection V3 V3 deriving (Show, Generic)

instance Out Projection where
instance Out Camera where

shotMatrix :: Projection -> Scalar -> Mat44
shotMatrix Perspective { fov, nearPlane, farPlane } screenRatio =
        mat44Perspective fov (realToFrac screenRatio) nearPlane farPlane
shotMatrix Ortho { width, near, far, shouldCenter } screenRatio =
        if shouldCenter
            then
                mat44Ortho (-(width/2)) (width/2) (-(height/2)) (height/2) near far
            else
                mat44Ortho 0 width 0 height near far
        where height = width / (realToFrac screenRatio)

worldViewMatrix :: Projection -> Scalar -> V3 -> V3 -> Mat44
worldViewMatrix proj screenRatio pos target =
        let worldMatrix = shotMatrix proj screenRatio
            camera = cameraPosMatrix pos target in
                mat44Mul worldMatrix camera

cameraPosMatrix :: V3 -> V3 -> Mat44
cameraPosMatrix pos target = mat44TranslateV (negate pos) rotated
    where current = v3 0 0 (-1)
          diff = target - pos
          axis = vcross diff current
          ang = vabsangle current diff
          rotated = if vnull axis || ang == 0 then mat44Identity else mat44RotateV axis (realToFrac ang) mat44Identity

cameraMatrix :: Camera -> Scalar -> Mat44
cameraMatrix (Camera proj center target) screenRatio = worldViewMatrix proj screenRatio center target

-- Drivers

data Route = Route V3 (Maybe Target) deriving Show

data Target = Target {
            tpos :: V3,
            moveTime :: Double,
            moveDuration :: Double
            } deriving Show

cameraCenter :: Route -> V3
cameraCenter (Route pos Nothing ) = pos
cameraCenter (Route pos (Just (Target tarpos time duration))) = vlinear (realToFrac (time / duration)) pos tarpos

checkTarget :: Route -> Double -> Route
checkTarget r@(Route pos Nothing) _ = r
checkTarget (Route pos (Just (Target tpos moveTime moveDuration))) delta = let time' = (moveTime + delta) in
    if time' > moveDuration
        then (Route tpos Nothing)
        else (Route pos (Just (Target tpos time' moveDuration)))
