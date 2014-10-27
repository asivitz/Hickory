{-# LANGUAGE NamedFieldPuns #-}

module Utils.Projection where

import Math.VectorMatrix
import Types.Types
import Math.Vector
import Engine.Scene.Scene

unproject :: V2 -> Scalar -> RenderInfo -> V3
unproject pos z (RenderInfo mat ss _) = lerpUnproject pos z mat (viewportFromSize ss)

-- Useful for transforming a difference in touch coordinates into
-- a world difference vector
unprojectDelta :: V2 -> Scalar -> RenderInfo -> V3
unprojectDelta p depth renderinfo = (unproject p depth renderinfo) - (unproject vZero depth renderinfo)
