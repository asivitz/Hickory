module Menus.Menus where

import Types.Types
import Graphics.Drawing
import Graphics.GLUtils
import Graphics.DrawText
import Math.Vector
import Types.Color

data Button a c = Button (RelativeRect Scalar a) ([c], Maybe (TransitionAction c))

data MenuDrawCommand = TextMenuDrawCommand TextCommand

type MenuItem a = (RelativePos Scalar a, RelativePos Scalar a, MenuDrawCommand)

data UIElement a c = UIElement (Maybe (Button a c)) [MenuItem a]

data MenuScreen a c = MenuScreen [UIElement a c] Scalar

makeLabel :: MenuItem a -> UIElement a c
makeLabel s = UIElement Nothing [s]

--

data TransitionStack t = TransitionStack ![t] !Double !(Maybe t)

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
