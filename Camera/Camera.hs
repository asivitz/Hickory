{-# LANGUAGE NamedFieldPuns #-}

module Camera.Camera where

import Math.Vector
import Math.Matrix
import Math.VectorMatrix

data Projection = Perspective {
                     fov :: Scalar,
                     nearPlane :: Scalar,
                     farPlane :: Scalar }
                  | Ortho {
                     width :: Scalar,
                     near :: Scalar,
                     far :: Scalar } deriving Show

data Target = Target {
            tpos :: V3,
            moveTime :: Double,
            moveDuration :: Double
            } deriving Show

data Route = Route V3 (Maybe Target) deriving Show

data Camera = Camera Projection Route deriving Show

getRoute :: Camera -> Route
getRoute (Camera _ r) = r

shotMatrix :: Projection -> Scalar -> Mat44
shotMatrix Perspective { fov, nearPlane, farPlane } screenRatio =
        mat44Perspective fov (realToFrac screenRatio) nearPlane farPlane
shotMatrix Ortho { width, near, far } screenRatio =
        mat44Ortho 0 width 0 height near far
        where height = width / (realToFrac screenRatio)

worldViewMatrix :: Projection -> Scalar -> V3 -> Mat44
worldViewMatrix proj screenRatio pos =
        let worldMatrix = shotMatrix proj screenRatio
            camera = cameraPosMatrix pos in
                mat44Mul worldMatrix camera

cameraPosMatrix :: V3 -> Mat44
cameraPosMatrix pos = 
    mat44TranslateV (negate pos) mat44Identity

cameraCenter :: Route -> V3
cameraCenter (Route pos Nothing ) = pos
cameraCenter (Route pos (Just (Target tarpos time duration))) = vlinear (realToFrac (time / duration)) pos tarpos


cameraMatrix :: Camera -> Scalar -> Mat44
cameraMatrix (Camera proj route) screenRatio = worldViewMatrix proj screenRatio (cameraCenter route)

checkTarget :: Route -> Double -> Route
checkTarget r@(Route pos Nothing) _ = r
checkTarget (Route pos (Just (Target tpos moveTime moveDuration))) delta = let time' = (moveTime + delta) in
    if time' > moveDuration
        then (Route tpos Nothing)
        else (Route pos (Just (Target tpos time' moveDuration)))
