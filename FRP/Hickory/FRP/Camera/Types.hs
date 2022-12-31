{-# LANGUAGE OverloadedLabels #-}

module Hickory.FRP.Camera.Types where

import Linear (V3, V4 (..), V2(..), (!*), (!*!), norm)
import GHC.Generics (Generic)
import Hickory.Math (Scalar, Mat44, rlerp, v3tov4, viewTarget)
import Hickory.Types (Size(..), aspectRatio)
import Hickory.Camera (Projection(..), shotMatrix)
import Control.Lens (has)
import Data.Generics.Labels ()

data Camera = Camera
  { focusPos     :: V3 Scalar
  , angleVec     :: V3 Scalar
  , up           :: V3 Scalar
  , projection   :: Projection
  } deriving Generic

perspectiveFocusPlaneSize :: Size Int -> V3 Scalar -> Scalar -> Size Scalar
perspectiveFocusPlaneSize (aspectRatio -> scrRat) angleVec fov = Size cameraFocusPlaneWidth cameraFocusPlaneHeight
  where
  cameraFocusPlaneHeight = tan (fov / 2) * norm angleVec * 2
  cameraFocusPlaneWidth = cameraFocusPlaneHeight * scrRat

cameraFocusPlaneSize :: Size Int -> Camera -> Size Scalar
cameraFocusPlaneSize size Camera {..} = case projection of
  Perspective fov _ _ -> perspectiveFocusPlaneSize size angleVec fov
  Ortho width _ _ _ -> Size width (width / aspectRatio size)

mkViewMat :: V3 Scalar -> V3 Scalar -> V3 Scalar -> Mat44 -- used to build the shadowmap
mkViewMat center towardCenter up
    = viewTarget (center - towardCenter) center up

cameraViewMat :: Camera -> Mat44
cameraViewMat Camera {..} = mkViewMat focusPos angleVec up

cameraProjMat :: Size Int -> Camera -> Mat44
cameraProjMat size Camera {..} = shotMatrix projection (aspectRatio size)

project :: Size Int -> Camera -> V3 Scalar -> V2 Scalar
project size@(Size scrW scrH) cs v =
  V2 (rlerp (x/w) (-1) 1 * realToFrac scrW) (rlerp (y/w) (-1) 1 * realToFrac scrH)
  where
  mat = cameraProjMat size cs !*! cameraViewMat cs
  V4 x y _ w = mat !* v3tov4 v 1

isOrthographic :: Camera -> Bool
isOrthographic = has (#projection . #_Ortho)
