{-# LANGUAGE NamedFieldPuns #-}

module Hickory.Utils.Projection where

import Hickory.Math.Matrix
import Hickory.Math.VectorMatrix
import Hickory.Types
import Hickory.Math.Vector
import Control.Lens ((^.))

unproject :: Mat44 -> Size Int -> Scalar -> V2 Scalar -> V2 Scalar
unproject mat ss z pos = lerpUnproject pos z mat (fmap realToFrac $ viewportFromSize ss) ^. _xy

-- Useful for transforming a difference in touch coordinates into
-- a world difference vector
unprojectDelta :: V2 Scalar -> Scalar -> Mat44 -> Size Int -> V2 Scalar
unprojectDelta p depth mat ss = (unproject mat ss depth p) - (unproject mat ss depth zero)

project :: Mat44 -> Size Int -> V3 Scalar -> V3 Scalar
project mat ss = viewProject mat (fmap realToFrac $ viewportFromSize ss)
