{-# LANGUAGE NamedFieldPuns #-}

module Systems.UICamera (make) where

import Engine.World
import Engine.System
import Math.Vector
import Control.Monad.State
import Data.IORef

import Camera.Camera

data SysData = SysData {
             camera :: Camera
             }

camera' uicam = do
        SysData { camera } <- getSysData uicam
        return $ Just camera

make :: SysMonad c IO (System c)
make = do
        uicam <- liftIO $ newIORef empty
        registerResource uiCamera (camera' uicam)
        return $ System (run uicam)

empty = SysData (Camera (Ortho 400 (-20) 1) (Route pZero Nothing))


run camera delta =
        do
            sd@SysData { camera = (Camera proj route) } <- getSysData camera
            let route' = checkTarget route delta
            putSysData camera sd { camera = (Camera proj route') }

{-initS camera = -}

