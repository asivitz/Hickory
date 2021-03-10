module UI.Elements where

import Hickory.Utils.Utils
import Hickory.Types
{-import UI.Stack-}
import Data.Maybe
import Hickory.Math.Vector
import Linear.Metric
import Linear (V2(..), V3(..))

{-type TransitionAction t = TransitionStack t -> TransitionStack t-}

{-data Button c t = Button (RelativeRect Scalar Scalar) ([c], Maybe (TransitionAction t))-}

{-data UIElement c t mi = UIElement (Maybe (Button c t)) [mi]-}

{-data MenuScreen c mi = MenuScreen [UIElement c (MenuScreen c mi) mi] Scalar-}

{-makeLabel :: mi -> UIElement c t mi-}
{-makeLabel s = UIElement Nothing [s]-}

data UIElement re model = UIElement re (V2 Scalar) (Maybe (Button model))
                        | FullscreenElement re

data Target = Circle Scalar
            | Box (Size Scalar)

data Interaction model = Tap (model -> model)
                 | Track (V2 Scalar -> model -> model)

data Button model = Button Target (Interaction model)

data TouchType = TouchUp Scalar | TouchMove | TouchDown
data Touch = Touch TouchType (V2 Scalar)

makeLabel :: re -> V2 Scalar -> UIElement re model
makeLabel re vec = UIElement re vec Nothing

gridPositions :: V2 Scalar -> Int -> Scalar -> Scalar -> [V2 Scalar]
gridPositions topLeft cols colSpacing rowSpacing =
        let row = map (\i -> topLeft + (V2 (colSpacing * (realToFrac i)) 0)) [0..(cols - 1)] in
            row ++ (gridPositions (topLeft + (V2 0 rowSpacing)) cols colSpacing rowSpacing)

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | y <- ys, x <- xs]

hitTarget :: V2 Scalar -> V2 Scalar -> Target -> Bool
hitTarget vec vec' (Circle scal) = norm (vec' - vec) < scal
hitTarget vec vec' (Box (size)) =
        let rect = (Rect vec' size)
            in posInRect vec rect

targetHitRelativeLocation :: V2 Scalar -> V2 Scalar -> Target -> Maybe (V2 Scalar)
targetHitRelativeLocation vec rvec (Circle rscal) = error "Not implemented"
targetHitRelativeLocation vec vec' (Box (rsize)) =
        let rect = (Rect vec' rsize)
            in relativePosInRect vec rect

applyTouch (Touch (TouchUp time) vec) ss (UIElement _ _ (Just (Button targ (Tap f))))
    | time < 0.4 = f
applyTouch (Touch touchtype vec) ss (UIElement _ vec' (Just (Button target (Track f))))
    = case targetHitRelativeLocation vec vec' target of
          Just v -> f v
          Nothing -> id
applyTouch _ _ _ = id

clickSurface :: (Size Int) -> [UIElement re model] -> (Touch -> model -> model) -> model -> Touch -> model
clickSurface ss xforms nohit model click@(Touch ttype vec) = case closestXForm of
                                               Nothing -> nohit click model
                                               Just x -> (applyTouch click ss x) model
        where closestXForm = listToMaybe $ filter hits xforms
              hits (UIElement _ rvec (Just (Button target _))) = hitTarget vec rvec target
              hits _ = False

-- Intervals

data Interval a = Interval a a

intervalIndices :: [Double]
intervalIndices = [5, 3, 1, 4, 3, 0]

intervals :: [Interval Double]
intervals = map (\i -> Interval (i * 0.1) (i * 0.1 + 0.5)) intervalIndices

pickInterval idx = intervals !! (idx `mod` 6)

constrainInterval :: Double -> Int -> Double
constrainInterval fraction idx = case (pickInterval idx) of
                                     Interval low high -> rlerpClamp fraction low high

slide :: Fractional a => Bool -> RelativeScalar a b -> RelativeScalar a b
slide True (RScal frac offset) = (RScal (frac * 0.5) offset)
slide False (RScal frac offset) = (RScal ((2 - frac) * 0.5) offset)


{-
processMenuStack :: RenderInfo -> V2 -> TransitionStack (MenuScreen ie mdc) -> Maybe (TransitionStack (MenuScreen ie mdc), [ie])
processMenuStack renderinfo@(RenderInfo _ ss _) pos transitionStk =
        let unproj = unproject pos (-5) renderinfo in
            case incomingScreen transitionStk of
                Just (MenuScreen elements _) ->
                    let acts = listToMaybe $ mapMaybe (\(UIElement mbutton _) ->
                            case mbutton of
                                Just (Button rrect actions) -> if (posInRect (v3tov2 unproj) (transformRect rrect ss))
                                                                then Just actions
                                                                else Nothing
                                Nothing -> Nothing) elements
                        in case acts of
                               Just (ies, action) -> Just (maybe transitionStk (\a -> a transitionStk ) action, ies)
                               Nothing -> Nothing
                Nothing -> Nothing
                -}
