-- |'Omniscient' style camera. Controls for panning, rotating, zooming.
-- Good for 3D editors

{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Hickory.FRP.Camera.Omniscient where

import qualified Reactive.Banana as B
import Hickory.FRP.CoreEvents (CoreEvents (..), concatTouchEvents)
import qualified Reactive.Banana.Frameworks as B
import Reactive.Banana ((<@>))
import Hickory.Math (Scalar, Mat44, viewTarget)
import Hickory.Types (Size (..), aspectRatio)
import Hickory.FRP.UI (trackTouches, TouchChange(..))
import Data.Maybe (mapMaybe)
import Hickory.FRP.Combinators (unionFirst)
import Hickory.Input (Key(..))
import Linear (rotate, axisAngle, V3 (..), V2 (..), normalize, (^*), cross, norm)
import Control.Lens ((<&>))
import Safe (headMay)
import GHC.Generics (Generic)
import Hickory.Camera (Projection(..), shotMatrix)

data CameraMoveMode = Pan | Rotate | Zoom
  deriving Eq

data CameraViewMode = OrthoTop | OrthoFront | PerspView
  deriving Eq

data CameraState = CameraState
  { viewMat  :: Mat44
  , projMat  :: Mat44
  , viewMode :: CameraViewMode
  , focusPos :: V3 Scalar
  , angleVec :: V3 Scalar
  , up       :: V3 Scalar
  } deriving Generic

omniscientCamera :: CoreEvents a -> B.MomentIO (B.Behavior CameraState)
omniscientCamera coreEvents = mdo
  (eChanges, _bTouches) <- trackTouches (concatTouchEvents coreEvents)

  let eOrthoTop = B.filterE (==Key'0) $ keyDown coreEvents
  let eOrthoFront = B.filterE (==Key'1) $ keyDown coreEvents

  let mode = (\cmd shift -> if cmd then Zoom else if shift then Pan else Rotate) <$> keyHeldB coreEvents Key'LeftSuper <*> keyHeldB coreEvents Key'LeftShift

  let clickMove = B.filterJust $ fmap headMay $ eChanges <&> mapMaybe \case
        LocTouch _ 0 v -> Just v
        _ -> Nothing
      clickStart = B.filterJust $ fmap headMay $ eChanges <&> mapMaybe \case
        AddTouch _ 0 v -> Just v
        _ -> Nothing
      clickEnd = B.filterJust $ fmap headMay $ eChanges <&> mapMaybe \case
        AddTouch _ 0 v -> Just v
        _ -> Nothing

      eRepositionCamera :: B.Event (V3 Scalar) = B.whenE ((==Pan) <$> mode) $
        let f (Size scrW scrH) upv (Size orthoW orthoH) (Just (start, focusPos, triple)) v =
              let angle = buildCameraAngleVec triple
                  xaxis = normalize $ cross (normalize angle) upv
                  yaxis = normalize $ cross xaxis (normalize angle)
                  (V2 vx vy) = v - start
              in focusPos - xaxis ^* (vx / realToFrac scrW * orthoW) + yaxis ^* (vy / realToFrac scrH * orthoH)
        in f <$> scrSizeB coreEvents <*> up <*> (cameraFocusPlaneSize <$> scrSizeB coreEvents <*> cameraAngleVec) <*> captured <@> clickMove

      eZoomCamera :: B.Event Scalar = B.whenE ((==Zoom) <$> mode) $ ((,) <$> captured <@> clickMove ) <&> \(Just (start, _, (_,zoom)), v) ->
        let V2 _vx vy = v - start
        in zoom - vy / 10

      eRotateCamera :: B.Event (Scalar,Scalar) = B.whenE ((==Rotate) <$> mode) $ ((,) <$> captured <@> clickMove ) <&> \(Just (start, _, ((zang,ele),_)), v) ->
        let V2 vx vy = v - start
        in (zang - vx / 100, ele - vy / 100)

  captured :: B.Behavior (Maybe (V2 Scalar, V3 Scalar, ((Scalar, Scalar), Scalar))) <- B.stepper Nothing $ unionFirst
    [ (\cfp ct ps -> Just (ps, cfp, ct)) <$> cameraFocusPos <*> cameraTriple <@> clickStart
    , Nothing <$ clickEnd
    ]

  cameraAngles <- B.stepper (-pi/4, -3*pi/4) $ unionFirst
    [ eRotateCamera
    , (0, pi) <$ eOrthoTop
    , (0, -pi/2) <$ eOrthoFront
    ]
  cameraZoom     :: B.Behavior Scalar      <- B.stepper 20 eZoomCamera
  cameraFocusPos :: B.Behavior (V3 Scalar) <- B.stepper (V3 0 0 0) eRepositionCamera
  let cameraTriple = (,) <$> cameraAngles <*> cameraZoom
  let buildCameraAngleVec ((zang,ele),zoom) = (^* zoom)
        . rotate (axisAngle (V3 0 0 1) zang)
        . rotate (axisAngle (V3 1 0 0) ele)
        $ V3 0 0 1

  cameraViewMode <- B.stepper PerspView $ unionFirst
    [ OrthoTop   <$ eOrthoTop
    , OrthoFront <$ eOrthoFront
    , PerspView  <$ eRotateCamera
    ]

  let up = cameraAngles <&> \(zang,ele)
        -> rotate (axisAngle (V3 0 0 1) zang)
         . rotate (axisAngle (V3 1 0 0) ele)
         $ V3 0 (-1) 0

  let cameraAngleVec :: B.Behavior (V3 Scalar) = buildCameraAngleVec <$> cameraTriple

  let viewMat = mkViewMat <$> cameraFocusPos <*> cameraAngleVec <*> up
      projMat = mkProjMat <$> scrSizeB coreEvents <*> cameraAngleVec <*> cameraViewMode
  pure $ CameraState <$> viewMat <*> projMat <*> cameraViewMode <*> cameraFocusPos <*> cameraAngleVec <*> up

cameraFocusPlaneSize :: Size Int -> V3 Scalar -> Size Scalar
cameraFocusPlaneSize (aspectRatio -> scrRat) angleVec = Size cameraFocusPlaneWidth cameraFocusPlaneHeight
  where
  cameraFocusPlaneHeight = tan (camFov / 2) * norm angleVec * 2
  cameraFocusPlaneWidth = cameraFocusPlaneHeight * scrRat

camFov :: Floating a => a
camFov = pi/4

mkViewMat :: V3 Scalar -> V3 Scalar -> V3 Scalar -> Mat44 -- used to build the shadowmap
mkViewMat center towardCenter up
    = viewTarget (center - towardCenter) center up


mkProjMat :: Size Int -> V3 Scalar -> CameraViewMode -> Mat44
mkProjMat size@(aspectRatio -> scrRat) angleVec = \case
  OrthoTop   -> orthoMat
  OrthoFront -> orthoMat
  PerspView  -> shotMatrix (Perspective camFov 0.1 400) scrRat
  where
  orthoMat = shotMatrix (Ortho width 0.1 400 True) scrRat
  (Size width _) = cameraFocusPlaneSize size angleVec
