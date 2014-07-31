{-# LANGUAGE NamedFieldPuns #-}

module Camera.Camera where

import Math.Vector
import Math.Matrix

data Projection = Perspective {
                     fov :: Float,
                     nearPlane :: Float,
                     farPlane :: Float }
                  | Ortho {
                     width :: Float,
                     near :: Float,
                     far :: Float }

data Target = Target {
            tpos :: Vec,
            moveTime :: Double,
            moveDuration :: Double
            }

data Route = Route Vec (Maybe Target)

data Camera = Camera Projection Route

shotMatrix :: Projection -> Float -> Mat44
shotMatrix Perspective { fov, nearPlane, farPlane } screenRatio =
        mat44Perspective fov screenRatio nearPlane farPlane
shotMatrix Ortho { width, near, far } screenRatio =
        mat44Ortho (negate hw) hw (negate hh) hh near far
        where hw = width / 2
              hh = (width / screenRatio) / 2

worldViewMatrix :: Projection -> Float -> Float -> Vec -> Mat44
worldViewMatrix proj screenRatio zoom pos =
        let worldMatrix = shotMatrix proj screenRatio
            camera = cameraPosMatrix zoom pos in
                mat44Mul worldMatrix camera

cameraPosMatrix :: Float -> Vec -> Mat44
cameraPosMatrix cameraZoom (V2 x y) = 
    mat44Translate (negate x) (negate y) (negate cameraZoom) $ mat44Identity

cameraCenter :: Route -> Vec
cameraCenter (Route pos Nothing ) = pos
cameraCenter (Route pos (Just (Target tarpos time duration))) = lerp (realToFrac (time / duration)) pos tarpos


cameraMatrix :: Camera -> Float -> Mat44
cameraMatrix (Camera proj route) screenRatio = worldViewMatrix proj screenRatio 0 (cameraCenter route)

checkTarget :: Route -> Double -> Route
checkTarget r@(Route pos Nothing) _ = r
checkTarget (Route pos (Just (Target tpos moveTime moveDuration))) delta = let time' = (moveTime + delta) in
    if time' > moveDuration
        then (Route tpos Nothing)
        else (Route pos (Just (Target tpos time' moveDuration)))
