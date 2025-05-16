{-# LANGUAGE OverloadedStrings #-}

module Hickory.Graphics.Mesh where

import Data.Foldable (toList)
import Hickory.Math.Vector
import Linear (V3(..))
import qualified Data.Vector.Storable as SV

-- Stock cube mesh

cubeFloats :: SV.Vector Scalar
cubeFloats = SV.fromList . concatMap toList $ verts
 where
  h     = 0.5
  l     = -h
  p1    = V3 l l l
  p2    = V3 h l l
  p3    = V3 h h l
  p4    = V3 l h l
  p5    = V3 l l h
  p6    = V3 h l h
  p7    = V3 h h h
  p8    = V3 l h h
  verts = [p1, p2, p3, p4, p5, p6, p7, p8]

cubeIndices :: SV.Vector Int
cubeIndices = SV.fromList [6, 7, 5, 4, 0, 7, 3, 6, 2, 5, 1, 0, 2, 3]
