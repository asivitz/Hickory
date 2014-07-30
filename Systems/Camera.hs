{-# LANGUAGE NamedFieldPuns #-}

module Systems.Simple (empty, make, SysData) where

import System
import Event
import Component
import Types


make camera = System (run camera) nullHandleEvent nullInit
empty = SysData (Vec2 0 0) Nothing

starTex = "filled_star.png"

checkTarget pos Nothing _ = (pos, Nothing)
checkTarget pos (Target tpos moveTime moveDuration) delta = let time' = (moveTime + delta) in
    if time' > moveDuration
        then (tpos, Nothing)
        else (pos, (Target tpos time' moveDuration))

run camera delta =
        do
            SysData { pos, target } <- getSysData
            let (pos', target') = checkTarget pos target
            putSysData SysData { pos', target' }

cameraCenter :: SysData -> Vec
cameraCenter SysData { pos, target = Nothing } = pos
cameraCenter SysData { pos, target = (Target tpos moveTime moveDuration) } = lerp (moveTime / moveDuration) pos tpos

{-
(define (camera-center)
  (let* ([base-pos
           (if (null? camera-target)
             camera-pos
             (pos2-lerp camera-pos camera-target (/ move-time move-duration)))]
         [scrolled-pos (if (null? scroll-buffer)
                         base-pos
                         (pos2+ base-pos scroll-buffer))]
         )
    (pos2+ base-pos (camera-scroll-clamp-func scroll-buffer base-pos))))
    -}

initS camera = do
        tid <- Textures.reserveTex texes starTex
        pid <- DrawText.reservePrinter dt texes "impact"
        nilla <- Draw.reserveShader draw ("Shader.vsh", "Shader.fsh")
        putSysData simple SysData { texid = tid, printer = pid, vanilla = nilla }

data Target = Target {
            pos :: Vec,
            moveTime :: UTCTime,
            moveDuration :: UTCTime
            }

data SysData = SysData {
             pos :: Vec,
             target :: Maybe Target
             }
