{-# LANGUAGE NamedFieldPuns #-}

module Utils.Projection where

import Math.Matrix
import Math.VectorMatrix
import Types.Types
import Math.Vector

unproject :: V2 Scalar -> Scalar -> Mat44 -> Size Int -> V3 Scalar
unproject pos z mat ss = lerpUnproject pos z mat (fmap realToFrac $ viewportFromSize ss)

-- Useful for transforming a difference in touch coordinates into
-- a world difference vector
unprojectDelta :: V2 Scalar -> Scalar -> Mat44 -> Size Int -> V3 Scalar
unprojectDelta p depth mat ss = (unproject p depth mat ss) - (unproject zero depth mat ss)
