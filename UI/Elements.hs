module UI.Elements where

import Utils.Utils
import Types.Types
{-import UI.Stack-}
import Data.Maybe
import Math.Vector

{-type TransitionAction t = TransitionStack t -> TransitionStack t-}

{-data Button c t = Button (RelativeRect Scalar Scalar) ([c], Maybe (TransitionAction t))-}

{-data UIElement c t mi = UIElement (Maybe (Button c t)) [mi]-}

{-data MenuScreen c mi = MenuScreen [UIElement c (MenuScreen c mi) mi] Scalar-}

{-makeLabel :: mi -> UIElement c t mi-}
{-makeLabel s = UIElement Nothing [s]-}

data UIElement re model = UIElement re (RelativeVec Scalar Scalar) (Maybe (Button model))

data Target = Circle (RelativeScalar Scalar Scalar)
            | Box (RelativeVec Scalar Scalar)

data Button model = TapButton Target (model -> model)
                  | TrackButton Target (V2 -> model -> model)

data TouchType = TouchUp Scalar | TouchMove | TouchDown
data Touch = Touch TouchType V2

makeLabel :: re -> RelativeVec Scalar Scalar -> UIElement re model
makeLabel re rvec = UIElement re rvec Nothing

gridPositions :: V2 -> Int -> Scalar -> Scalar -> [V2]
gridPositions topLeft cols colSpacing rowSpacing =
        let row = map (\i -> topLeft + (v2 (colSpacing * (realToFrac i)) 0)) [0..(cols - 1)] in
            row ++ (gridPositions (topLeft + (v2 0 rowSpacing)) cols colSpacing rowSpacing)

hitTarget :: V2 -> Size Int -> RelativeVec Scalar Scalar -> Target -> Bool
hitTarget vec ss@(Size w h) rvec (Circle rscal) = vmag ((v3tov2 $ screenPos ss rvec) - vec) < (transform rscal w)
hitTarget vec ss rvec (Box (rsize)) =
        let rect = transformRect (RRect rvec rsize) ss
            in posInRect vec rect

targetHitRelativeLocation :: V2 -> Size Int -> RelativeVec Scalar Scalar -> Target -> Maybe V2
targetHitRelativeLocation vec ss@(Size w h) rvec (Circle rscal) = error "Not implemented"
targetHitRelativeLocation vec ss rvec (Box (rsize)) =
        let rect = transformRect (RRect rvec rsize) ss
            in relativePosInRect vec rect

itemHitTarget :: Touch -> (Size Int) -> UIElement re model -> Maybe (model -> model)
itemHitTarget (Touch touchtype vec) ss (UIElement _ rvec Nothing) = Nothing
itemHitTarget (Touch (TouchUp time) vec) ss (UIElement _ rvec (Just (TapButton target f)))
        | time < 0.4 =
            if hitTarget vec ss rvec target
                then Just f
                else Nothing
        | otherwise = Nothing
itemHitTarget (Touch touchtype vec) ss (UIElement _ rvec (Just (TrackButton target f))) =
        case targetHitRelativeLocation vec ss rvec target of
            Just v -> Just (f v)
            _ -> Nothing
itemHitTarget _ _ _ = Nothing

clickSurface :: (Size Int) -> [UIElement re model] -> (model -> model) -> model -> Touch -> model
clickSurface ss xforms nohit model click = case listToMaybe $ mapMaybe (itemHitTarget click ss) xforms of
                                               Just f -> f model
                                               Nothing -> nohit model

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
