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
import Hickory.Math (Scalar)
import Hickory.Types (Size (..))
import Hickory.FRP.UI (trackTouches, TouchChange(..))
import Data.Maybe (mapMaybe)
import Hickory.FRP.Combinators (unionFirst)
import Hickory.Input (Key(..))
import Linear (rotate, axisAngle, V3 (..), V2 (..), normalize, (^*), cross)
import Control.Lens ((<&>))
import Safe (headMay)
import Hickory.Camera (Projection(..), Camera (..), perspectiveFocusPlaneSize)

data CameraMoveMode = Pan | Rotate | Zoom
  deriving Eq

data CameraViewMode = OrthoTop | OrthoFront | OrthoRight | OrthoLeft | OrthoBack | PerspView
  deriving Eq

omniscientCamera :: CoreEvents a -> B.MomentIO (B.Behavior Camera)
omniscientCamera coreEvents = mdo
  (eChanges, _bTouches) <- trackTouches (concatTouchEvents coreEvents)

  let eOrthoTop = B.filterE (==Key'0) $ keyDown coreEvents
      eOrthoFront = B.filterE (==Key'1) $ keyDown coreEvents
      eOrthoRight = B.filterE (==Key'2) $ keyDown coreEvents
      eOrthoLeft  = B.filterE (==Key'3) $ keyDown coreEvents
      eOrthoBack  = B.filterE (==Key'4) $ keyDown coreEvents

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
        in f <$> scrSizeB coreEvents <*> up <*> orthoSize <*> captured <@> clickMove

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
    , (pi/2, -pi/2) <$ eOrthoRight
    , (-pi/2, -pi/2) <$ eOrthoLeft
    , (pi, -pi/2) <$ eOrthoBack
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
    , OrthoLeft  <$ eOrthoLeft
    , OrthoRight <$ eOrthoRight
    , OrthoBack  <$ eOrthoBack
    , PerspView  <$ eRotateCamera
    ]

  let up = cameraAngles <&> \(zang,ele)
        -> rotate (axisAngle (V3 0 0 1) zang)
         . rotate (axisAngle (V3 1 0 0) ele)
         $ V3 0 (-1) 0

  let cameraAngleVec :: B.Behavior (V3 Scalar) = buildCameraAngleVec <$> cameraTriple

      projection = let f (Size width _) = \case
                        True -> Ortho width 0.1 400 True
                        False -> Perspective camFov 0.1 400
                   in f <$> orthoSize <*> (isOrthographicViewMode <$> cameraViewMode)

      orthoSize = perspectiveFocusPlaneSize <$> scrSizeB coreEvents <*> cameraAngleVec <*> pure camFov

  pure $ Camera <$> cameraFocusPos <*> cameraAngleVec <*> up <*> projection

isOrthographicViewMode :: CameraViewMode -> Bool
isOrthographicViewMode = \case
  PerspView -> False
  _ -> True

camFov :: Floating a => a
camFov = pi/4
