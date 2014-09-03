{-# LANGUAGE NamedFieldPuns #-}

module Systems.WorldCamera (make) where

import Engine.World
import Engine.System
import Math.Vector
import Control.Monad.State
import Data.IORef

import Camera.Camera

data SysData = SysData {
             camera :: Camera
             }

camera' worldcam = do
        SysData { camera } <- getSysData worldcam
        return $ Just camera

setWP worldcam proj = do
        sd@SysData { camera = (Camera _ route) } <- getSysData worldcam
        putSysData worldcam sd { camera = (Camera proj route) }

make :: SysMonad c IO (System c)
make = do
        worldcam <- liftIO $ newIORef empty
        registerResource sysCon worldCamera (camera' worldcam)
        registerResource sysCon setWorldProjection (setWP worldcam)
        return $ System (run worldcam)

empty = SysData (Camera (Perspective (pi / 2) 1 100) (Route pZero Nothing))

run :: IORef SysData -> Double -> SysMonad r IO ()
run camera delta =
        do
            sd@SysData { camera = (Camera proj route) } <- getSysData camera
            let route' = checkTarget route delta
            putSysData camera sd { camera = (Camera proj route') }

{-initS camera = -}

