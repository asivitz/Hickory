{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hickory.Camera where

import Hickory.Math.Vector
import Hickory.Math.Matrix
import Linear (V3, lerp, V4 (..), V2(..), (!*), (!*!), norm)
import GHC.Generics (Generic)
import Hickory.Types (Size(..), aspectRatio)
import Control.Lens (has)
import Data.Generics.Labels ()
import Data.Text (Text)

data Projection = Perspective
  { fov       :: Scalar -- Vertical fov
  , nearPlane :: Scalar
  , farPlane  :: Scalar
  }
  | Ortho
  { width        :: Scalar
  , near         :: Scalar
  , far          :: Scalar
  , shouldCenter :: Bool
  } deriving (Generic, Show)

shotMatrix :: Projection -> Scalar -> Mat44
shotMatrix Perspective { fov, nearPlane, farPlane } screenRatio =
  perspectiveProjection (realToFrac screenRatio) fov nearPlane farPlane
shotMatrix Ortho { width, near, far, shouldCenter } screenRatio = if shouldCenter
  -- As per Vulkan convention, the top of the screen toward -Y
  then orthographicProjection (-(width / 2)) (width / 2) (height / 2) (-(height / 2)) near far
  else orthographicProjection 0 width height 0 near far
  where height = width / realToFrac screenRatio

-- Drivers

data Route = Route (V3 Scalar) (Maybe Target) deriving Show

data Target = Target
  { tpos :: V3 Scalar
  , moveTime :: Scalar
  , moveDuration :: Scalar
  } deriving Show

cameraCenter :: Route -> V3 Scalar
cameraCenter (Route pos Nothing ) = pos
cameraCenter (Route pos (Just (Target tarpos time duration))) = lerp (realToFrac (time / duration)) tarpos pos

checkTarget :: Route -> Scalar -> Route
checkTarget r@(Route _pos Nothing) _ = r
checkTarget (Route pos (Just (Target tpos moveTime moveDuration))) delta =
  let time' = (moveTime + delta)
  in  if time' > moveDuration then Route tpos Nothing else Route pos (Just (Target tpos time' moveDuration))

-- Camera
data Camera = Camera
  { focusPos     :: V3 Scalar
  , angleVec     :: V3 Scalar -- Vec toward the focus position
  , up           :: V3 Scalar
  , projection   :: Projection
  , name         :: Text      -- For interpolating shot changes
  } deriving (Generic, Show)

cameraNear :: Camera -> Scalar
cameraNear camera = case projection camera of
  Perspective {..} -> nearPlane
  Ortho {..} -> near

cameraFar :: Camera -> Scalar
cameraFar camera = case projection camera of
  Perspective {..} -> farPlane
  Ortho {..} -> far

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

cameraPos :: Camera -> V3 Scalar
cameraPos Camera {..} = focusPos - angleVec
