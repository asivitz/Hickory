{-# LANGUAGE NamedFieldPuns #-}

module Utils.Projection where

import Engine.World
import Engine.System
import Math.VectorMatrix
import Types.Types
import Math.Vector

doLerpUnproject :: V2 -> Scalar -> SysMonad r IO V3
doLerpUnproject pos z = do
        RPC { _screenSize, _drawnWorldMatrix } <- getRPC systemContext
        ss <- _screenSize
        worldmat <- _drawnWorldMatrix
        return $ lerpUnproject pos z worldmat (viewportFromSize ss)
