{-# LANGUAGE NamedFieldPuns #-}

module Hickory.Utils.Projection where

import Hickory.Math.Matrix
import Hickory.Math.VectorMatrix
import Hickory.Types
import Hickory.Math.Vector

unproject :: V2 Scalar -> Scalar -> Mat44 -> Size Int -> V3 Scalar
unproject pos z mat ss = lerpUnproject pos z mat (fmap realToFrac $ viewportFromSize ss)

-- Useful for transforming a difference in touch coordinates into
-- a world difference vector
unprojectDelta :: V2 Scalar -> Scalar -> Mat44 -> Size Int -> V3 Scalar
unprojectDelta p depth mat ss = (unproject p depth mat ss) - (unproject zero depth mat ss)

viewport :: Real a => Size a -> V4 Scalar
viewport (Size w h) = V4 0 0 (realToFrac w) (realToFrac h)
