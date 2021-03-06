module UI.Stack where

data TransitionStack t = TransitionStack ![t] !Double !(Maybe t)

transitionTime :: TransitionStack t -> Double
transitionTime (TransitionStack _ time _) = time

emptyTransitionStack = TransitionStack [] 0 Nothing

pushScreen screen (TransitionStack stk time leaving) = (TransitionStack (screen:stk) 0 Nothing)

popScreen ts@(TransitionStack stk time leaving) =
        case stk of
            (x:xs) -> (TransitionStack xs 0 (Just x))
            _ -> error "Can't pop empty menu stack."

canPop :: TransitionStack t -> Bool
canPop (TransitionStack [] _ _) = False
canPop (TransitionStack [_] _ _) = False
canPop (TransitionStack _ _ _) = True

incomingScreen :: TransitionStack t -> Maybe t
incomingScreen (TransitionStack [] _ _) = Nothing
incomingScreen (TransitionStack (x:_) _ _) = Just x

leavingScreen :: TransitionStack t -> Maybe t
leavingScreen (TransitionStack _ _ (Just x)) = Just x
leavingScreen (TransitionStack (x:y:_) _ Nothing) = Just y
leavingScreen (TransitionStack _ _ _) = Nothing
