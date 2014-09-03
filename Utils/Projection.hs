{-# LANGUAGE NamedFieldPuns #-}

module Utils.Projection where

import Engine.World
import Engine.System
import Math.VectorMatrix
import Types.Types
import Math.Vector

doLerpUnproject :: V2 -> Scalar -> SysMonad r IO V3
doLerpUnproject pos z = do
        RSC { _screenSize, _drawnWorldMatrix } <- getRSC systemContext
        ss <- _screenSize
        worldmat <- _drawnWorldMatrix
        return $ lerpUnproject pos z worldmat (viewportFromSize ss)
