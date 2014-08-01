{-# LANGUAGE NamedFieldPuns #-}

module Systems.WorldCamera (empty, make, SysData(..)) where

import Engine.System
import Math.Vector

import Camera.Camera

data SysData = SysData {
             camera :: Camera
             }

make camera = System (run camera) nullHandleEvent nullInit
empty = SysData (Camera (Perspective (pi / 2) 1 100) (Route pZero Nothing))


run camera delta =
        do
            sd@SysData { camera = (Camera proj route) } <- getSysData camera
            let route' = checkTarget route delta
            putSysData camera sd { camera = (Camera proj route') }

{-initS camera = -}

