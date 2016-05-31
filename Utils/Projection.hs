{-# LANGUAGE NamedFieldPuns #-}

module Utils.Projection where

import Math.Matrix
import Math.VectorMatrix
import Types.Types
import Math.Vector
import Engine.Scene.Scene

unproject :: V2 -> Scalar -> Mat44 -> Size Int -> V3
unproject pos z mat ss = lerpUnproject pos z mat (viewportFromSize ss)

-- Useful for transforming a difference in touch coordinates into
-- a world difference vector
unprojectDelta :: V2 -> Scalar -> Mat44 -> Size Int -> V3
unprojectDelta p depth mat ss = (unproject p depth mat ss) - (unproject vZero depth mat ss)
