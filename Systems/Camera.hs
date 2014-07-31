{-# LANGUAGE NamedFieldPuns #-}

module Systems.Camera (empty, make, SysData(..)) where

import Engine.System
import Math.Vector
import Math.Matrix

import Camera.Camera

data SysData = SysData {
             camera :: Camera
             }

make camera = System (run camera) nullHandleEvent nullInit
empty = SysData (Camera (Ortho 400 (-20) 1) (Route pZero Nothing))
{-empty = SysData (Camera (Perspective (pi / 2) 1 100) (Route (V2 0 0) Nothing))-}


run camera delta =
        do
            sd@SysData { camera = (Camera proj route) } <- getSysData camera
            let route' = checkTarget route delta
            putSysData camera sd { camera = (Camera proj route') }

{-initS camera = -}

