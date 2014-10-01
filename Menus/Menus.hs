module Menus.Menus where

import Types.Types
import Graphics.Drawing
import Graphics.GLUtils
import Graphics.DrawText
import Math.Vector
import Types.Color

data Button c t = Button (RelativeRect Scalar Scalar) ([c], Maybe (TransitionAction t))

type MenuItem dc = (Bool -> Double -> RelativeVec Scalar Scalar, Bool -> Double -> dc)

data UIElement c t dc = UIElement (Maybe (Button c t)) [MenuItem dc]

data MenuScreen c dc = MenuScreen [UIElement c (MenuScreen c dc) dc] Scalar

makeLabel :: MenuItem dc -> UIElement c t dc
makeLabel s = UIElement Nothing [s]

--

data TransitionStack t = TransitionStack ![t] !Double !(Maybe t)

transitionTime :: TransitionStack t -> Double
transitionTime (TransitionStack _ time _) = time

emptyTransitionStack = TransitionStack [] 0 Nothing

type TransitionAction t = TransitionStack t -> TransitionStack t

pushScreen screen (TransitionStack stk time leaving) = (TransitionStack (screen:stk) 0 Nothing)

popScreen ts@(TransitionStack stk time leaving) =
        case stk of
            (x:xs) -> (TransitionStack xs 0 (Just x))
            _ -> error "Can't pop empty menu stack."

incomingScreen :: TransitionStack t -> Maybe t
incomingScreen (TransitionStack [] _ _) = Nothing
incomingScreen (TransitionStack (x:_) _ _) = Just x

leavingScreen :: TransitionStack t -> Maybe t
leavingScreen (TransitionStack _ _ (Just x)) = Just x
leavingScreen (TransitionStack (x:y:_) _ Nothing) = Just y
leavingScreen (TransitionStack _ _ _) = Nothing
