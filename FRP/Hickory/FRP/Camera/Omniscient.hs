-- |'Omniscient' style camera. Controls for panning, rotating, zooming.
-- Good for 3D editors

{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}

module Hickory.FRP.Camera.Omniscient where

-- import Hickory.FRP.CoreEvents (CoreEvents (..), concatTouchEvents)
import Hickory.Math (Scalar)
import Hickory.Types (Size (..))
import Data.Maybe (fromMaybe, isJust)
import Hickory.Input (Key(..), InputFrame(..))
import Linear (rotate, axisAngle, V3 (..), V2 (..), normalize, (^*), cross, zero)
import Control.Lens ((<&>), set)
import Safe (headMay)
import Hickory.Camera (Projection(..), Camera (..), perspectiveFocusPlaneSize)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import qualified Data.Enum.Set as ES
import Control.Monad (mfilter)
import Control.Applicative (Alternative (..))
import GHC.Generics (Generic)

data CameraMoveMode = Pan | Rotate | Zoom
  deriving Eq

data CameraViewMode = OrthoTop | OrthoFront | OrthoRight | OrthoLeft | OrthoBack | PerspView
  deriving Eq

data Omniscient = Omniscient
  { captured :: Maybe (V2 Scalar, V3 Scalar, ((Scalar, Scalar), Scalar))
  , zoom     :: Scalar
  , focusPos :: V3 Scalar
  , angles   :: (Scalar, Scalar)
  , viewMode :: CameraViewMode
  } deriving (Generic)

omniscientCamera :: IO (V3 Scalar -> IO (), Size Int -> InputFrame -> IO Camera)
omniscientCamera = do
  stateRef <- newIORef $ Omniscient Nothing 20 zero (-pi/4, -3*pi/4) PerspView

  pure . (modifyIORef' stateRef . set #focusPos,) $ \size inFr -> do
    st <- readIORef stateRef
    let keyPressed k = ES.member k inFr.pressedKeys
        keyHeld k    = ES.member k inFr.heldKeys

    let eOrthoTop   = keyPressed Key'0
        eOrthoFront = keyPressed Key'1
        eOrthoRight = keyPressed Key'2
        eOrthoLeft  = keyPressed Key'3
        eOrthoBack  = keyPressed Key'4
        superHeld = keyHeld Key'LeftSuper || keyHeld Key'RightSuper
        mode = if | superHeld -> Zoom
                  | keyHeld Key'LeftShift -> Pan
                  | otherwise -> Rotate

    let buildCameraAngleVec ((zang,ele),z) = (^* z)
          . rotate (axisAngle (V3 0 0 1) zang)
          . rotate (axisAngle (V3 1 0 0) ele)
          $ V3 0 0 1

    let up = let (zang,ele) = st.angles
             in rotate (axisAngle (V3 0 0 1) zang)
              . rotate (axisAngle (V3 1 0 0) ele)
              $ V3 0 (-1) 0
    let oldCameraAngleVec = buildCameraAngleVec (st.angles, st.zoom)
        orthoSize = perspectiveFocusPlaneSize size oldCameraAngleVec camFov

    let clickMove  = fst <$> headMay inFr.touchesLoc
        clickStart = fst <$> headMay inFr.touchesDown
        clickEnd   = headMay inFr.touchesUp

        eRepositionCamera :: Maybe (V3 Scalar) = mfilter (const $ mode == Pan) $
          let f (start, focusPos, triple) v =
                let angle = buildCameraAngleVec triple
                    xaxis = normalize $ cross (normalize angle) up
                    yaxis = normalize $ cross xaxis (normalize angle)
                    (V2 vx vy) = v - start
                in focusPos - xaxis ^* (vx / realToFrac size.width * orthoSize.width) + yaxis ^* (vy / realToFrac size.height * orthoSize.height)
          in f <$> st.captured <*> clickMove

        eZoomCamera :: Maybe Scalar = mfilter (const $ mode == Zoom) $ ((,) <$> st.captured <*> clickMove ) <&> \((start, _, (_,zoom)), v) ->
          let V2 _vx vy = v - start
          in zoom - vy / 10

        eRotateCamera :: Maybe (Scalar,Scalar) = case (,) <$> st.captured <*> clickMove of
          Just ((start, _, ((zang,ele),_)), v) ->
            let V2 vx vy = v - start
            in if mode == Rotate
                  && ( abs vx > 0.25
                      && abs vy > 0.25)
                then Just (zang - vx / 100, ele - vy / 100)
                else Nothing
          Nothing -> Nothing

    let angles =
          if | eOrthoTop -> (0, pi)
             | eOrthoFront -> (0, -pi/2)
             | eOrthoRight -> (pi/2, -pi/2)
             | eOrthoLeft -> (-pi/2, -pi/2)
             | eOrthoBack -> (pi, -pi/2)
             | otherwise -> fromMaybe st.angles eRotateCamera

    let zoom = fromMaybe st.zoom eZoomCamera
        focusPos = fromMaybe st.focusPos eRepositionCamera
        cameraTriple = (angles, zoom)

    let captured :: Maybe (V2 Scalar, V3 Scalar, ((Scalar, Scalar), Scalar)) =
          if isJust clickEnd
          then Nothing
          else (clickStart <&> \ps -> (ps, focusPos, cameraTriple)) <|> st.captured

    let viewMode =
          if | eOrthoTop -> OrthoTop
             | eOrthoFront -> OrthoFront
             | eOrthoLeft -> OrthoLeft
             | eOrthoRight -> OrthoRight
             | eOrthoBack -> OrthoBack
             | otherwise -> fromMaybe st.viewMode (PerspView <$ eRotateCamera)

    let cameraAngleVec :: V3 Scalar = buildCameraAngleVec cameraTriple

        projection = if isOrthographicViewMode viewMode
                     then Ortho orthoSize.width 0.1 400 True
                     else Perspective camFov 0.1 400
    writeIORef stateRef Omniscient {..}

    pure $ Camera focusPos cameraAngleVec up projection "Omniscient" Nothing

isOrthographicViewMode :: CameraViewMode -> Bool
isOrthographicViewMode = \case
  PerspView -> False
  _ -> True

camFov :: Floating a => a
camFov = pi/4
