{-# LANGUAGE NamedFieldPuns #-}

module Hickory.Utils.Projection where

import Hickory.Math.VectorMatrix
import Hickory.Types
import Control.Lens ((^.))
import Linear (V2, V3(..), _xy, zero, M44)

unproject :: Fractional a => M44 a -> Size Int -> a -> V2 a -> V2 a
unproject mat ss z pos = lerpUnproject pos z mat (realToFrac <$> viewportFromSize ss) ^. _xy

-- Useful for transforming a difference in touch coordinates into
-- a world difference vector
unprojectDelta :: Fractional a => V2 a -> a -> M44 a -> Size Int -> V2 a
unprojectDelta p depth mat ss = unproject mat ss depth p - unproject mat ss depth zero

project :: Fractional a => M44 a -> Size Int -> V3 a -> V3 a
project mat ss = viewProject mat (realToFrac <$> viewportFromSize ss)
