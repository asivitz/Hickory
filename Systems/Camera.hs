{-# LANGUAGE NamedFieldPuns #-}

module Systems.Camera (empty, make, SysData) where

import Engine.System
import Math.Vector
import Math.Matrix

data CameraShot = PerspectiveShot {
                     fov :: Float,
                     nearPlane :: Float,
                     farPlane :: Float }
                  | OrthoShot {
                     width :: Float,
                     near :: Float,
                     far :: Float }

data SysData = SysData {
             pos :: Vec,
             target :: Maybe Target,
             cameraShot :: CameraShot
             }

make camera = System (run camera) nullHandleEvent nullInit
empty = SysData (V2 0 0) Nothing (PerspectiveShot (pi / 2) 1 100)

shotMatrix :: CameraShot -> Float -> Mat44
shotMatrix (PerspectiveShot fov near far) screenRatio =
        mat44Perspective fov screenRatio near far
shotMatrix (OrthoShot width near far) screenRatio =
        mat44Ortho (negate hw) hw (negate hh) hh near far
        where hw = width / 2
              hh = (width / screenRatio) / 2

worldViewMatrix :: CameraShot -> Float -> Float -> Vec -> Mat44
worldViewMatrix shot screenRatio zoom pos =
        let worldMatrix = shotMatrix shot screenRatio
            camera = cameraPosMatrix zoom pos in
                mat44Mul worldMatrix camera

cameraPosMatrix :: Float -> Vec -> Mat44
cameraPosMatrix cameraZoom (V2 x y) = 
    mat44Translate (negate x) (negate y) (negate cameraZoom) $ mat44Identity

cameraMatrix :: SysData -> Float -> Mat44
cameraMatrix sd@SysData { cameraShot } screenRatio = worldViewMatrix cameraShot screenRatio 0 (cameraCenter sd)

checkTarget pos Nothing _ = (pos, Nothing)
checkTarget pos (Just (Target tpos moveTime moveDuration)) delta = let time' = (moveTime + delta) in
    if time' > moveDuration
        then (tpos, Nothing)
        else (pos, Just (Target tpos time' moveDuration))

run camera delta =
        do
            sd@SysData { pos, target } <- getSysData camera
            let (pos', target') = checkTarget pos target delta
            putSysData camera sd { pos = pos', target = target' }

cameraCenter :: SysData -> Vec
cameraCenter SysData { pos, target = Nothing } = pos
cameraCenter SysData { pos, target = Just (Target tpos moveTime moveDuration) } = lerp (realToFrac (moveTime / moveDuration)) pos tpos

{-initS camera = -}

data Target = Target {
            tpos :: Vec,
            moveTime :: Double,
            moveDuration :: Double
            }
