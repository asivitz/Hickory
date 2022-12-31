module Hickory.FRP.Camera.FirstPerson where

import Reactive.Banana (MonadMoment, Behavior, (<@), unions, accumB)
import Hickory.Math (clamp)
import Hickory.Types (Size(..))
import Linear (V2 (..), V3 (..), cross, (^*), normalize, rotate, axisAngle)
import Hickory.FRP.CoreEvents (CoreEvents(..))
import Data.Fixed (mod')
import Data.Functor ((<&>))
import Hickory.Input (Key(..))
import Hickory.FRP.Editor.General (mkCursorLoc)
import Hickory.Camera (Projection(..), Camera (..))

firstPersonCamera :: MonadMoment m => CoreEvents a -> m (Behavior Camera)
firstPersonCamera coreEvents = do
  cursorLoc <- mkCursorLoc coreEvents

  let mkRotationAngs (Size _w h) (V2 x y) =
        let lrang = mod' (x/30) (pi * 2)
            udang = clamp ((y - realToFrac h/2) / 30) (-pi/2) (pi/2) + pi/2
        in (lrang, udang)
      rotationAngs = mkRotationAngs <$> scrSizeB coreEvents <*> cursorLoc
      lookDir = rotationAngs <&> \(lrang,udang) ->
          rotate (axisAngle (V3 0 0 1) lrang)
        . rotate (axisAngle (V3 1 0 0) udang)
        $ V3 0 0 1
      up = rotationAngs <&> \(lrang,udang)
        -> rotate (axisAngle (V3 0 0 1) lrang)
         . rotate (axisAngle (V3 1 0 0) udang)
         $ V3 0 (-1) 0

  camPos <- accumB (V3 0 0 0) $ unions
    [ (\u v upv -> v - cross (normalize u) upv ^* 0.1) <$> lookDir <*> up <@ keyDownOrHeld coreEvents Key'A
    , (\u v upv -> v + cross (normalize u) upv ^* 0.1) <$> lookDir <*> up <@ keyDownOrHeld coreEvents Key'D
    , (\u v -> v + u ^* 0.1) <$> lookDir <@ keyDownOrHeld coreEvents Key'W
    , (\u v -> v - u ^* 0.1) <$> lookDir <@ keyDownOrHeld coreEvents Key'S
    , (+ V3 0 0 (-0.1)) <$ keyDownOrHeld coreEvents Key'X
    , (+ V3 0 0 0.1)  <$ keyDownOrHeld coreEvents Key'E
    ]

  let focusPos = (+) <$> camPos <*> lookDir
      angleVec = negate <$> lookDir
      projection = Perspective (pi/2) 0.1 400

  pure $ Camera <$> focusPos <*> angleVec <*> up <*> pure projection

{-
firstPersonLook
  :: MonadMoment m
  => Behavior (Size Int) -> Behavior (V2 Scalar) -> CoreEvents a -> m (Behavior Mat44)
firstPersonLook sizeB cursorLoc coreEvents = do
  let mkRotationAngs (Size _w h) (V2 x y) =
        let lrang = mod' (x/30) (pi * 2)
            udang = clamp ((y - realToFrac h/2) / 30) (-pi/2) (pi/2) + pi/2
        in (lrang, udang)
      mkMat p (lrang, udang) =
            mkRotation (V3 1 0 0) udang
        !*! mkRotation (V3 0 0 1) lrang
        !*! mkTranslation (-p)
      rotationAngs = mkRotationAngs <$> sizeB <*> cursorLoc
      lookDir = rotationAngs <&> \(lrang, udang) -> ( mkRotation (V3 0 0 1) (-lrang) -- inverse of the viewProj mat
                                                  !*! mkRotation (V3 1 0 0) (-udang)
                                                  !*  V4 0 0 1 1) ^. _xyz -- +z b/c that's how projection mat is set up

  manualCamera <- accumB (V3 0 0 1) $ unions
    [ (\u v -> v - cross (normalize u) (V3 0 0 1) ^* 0.1) <$> lookDir <@ keyDownOrHeld coreEvents Key'A
    , (\u v -> v + cross (normalize u) (V3 0 0 1) ^* 0.1) <$> lookDir <@ keyDownOrHeld coreEvents Key'D
    , (\u v -> v + u ^* 0.1) <$> lookDir <@ keyDownOrHeld coreEvents Key'W
    , (\u v -> v - u ^* 0.1) <$> lookDir <@ keyDownOrHeld coreEvents Key'S
    , (+ V3 0 0 (-0.1)) <$ keyDownOrHeld coreEvents Key'X
    , (+ V3 0 0 0.1)  <$ keyDownOrHeld coreEvents Key'E
    ]
  pure $ mkMat <$> manualCamera <*> rotationAngs
  -}
