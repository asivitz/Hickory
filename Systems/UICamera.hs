{-# LANGUAGE NamedFieldPuns #-}

module Systems.UICamera (empty, make, SysData(..)) where

import Engine.System
import Math.Vector

import Camera.Camera

data SysData = SysData {
             camera :: Camera
             }

make camera = System (run camera) nullInit
empty = SysData (Camera (Ortho 400 (-20) 1) (Route pZero Nothing))


run camera delta =
        do
            sd@SysData { camera = (Camera proj route) } <- getSysData camera
            let route' = checkTarget route delta
            putSysData camera sd { camera = (Camera proj route') }

{-initS camera = -}

