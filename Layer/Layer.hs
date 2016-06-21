{-# LANGUAGE NamedFieldPuns #-}

module Layer.Layer where

import Data.List

type Layer s i = s -> [i] -> s

type MonadicLayer m s i = s -> [i] -> m s

constructMonadicLayer :: Monad m => (s' -> s -> m s) -> Layer s' i -> MonadicLayer m (s, s') i
constructMonadicLayer stepf nextLayer (lay1, lay2) msg1s = do
        let lay2' = nextLayer lay2 msg1s
        lay1' <- stepf lay2' lay1
        return (lay1', lay2')

splitInput :: (s' -> s -> i -> s) -> (s, s') -> i -> (s, s')
splitInput f (s, s') i = (f s' s i, s')

splitStep :: (s' -> s -> s) -> (s, s') -> (s, s')
splitStep f (s, s') = (f s' s, s')

wrap :: Layer s' i -> Layer (s, s') i
wrap nextLayer (s, s') i = (s, nextLayer s' i)

applyInput :: (s -> i -> s) -> Layer s i -> Layer s i
applyInput inputf nextLayer s msgs = foldl' inputf (nextLayer s msgs) msgs

mapState :: (s -> s) -> Layer s i -> Layer s i
mapState f layer s i = f $ layer s i

mapInput :: (s -> j -> [i]) -> Layer s i -> Layer s j
mapInput xformer layer s is = layer s $ concatMap (xformer s) is

-- Debug Layer
data DebugMsg = FlipDebug
              | StepForward
              | JumpForward
              | StepBackward
              | JumpBackward

data DebugState model = DebugState {
             debug :: Bool,
             modelStack :: [model],
             stackIndex :: Int
             }
             deriving (Show)

mkDebugState = DebugState False [] 0

debugInput :: b -> DebugState b -> DebugMsg -> DebugState b
debugInput model debugstate@DebugState { debug, stackIndex, modelStack } input = state'
        where state' = case input of
                         FlipDebug -> if debug
                                          then debugstate { debug = not debug, stackIndex = 0, modelStack = drop stackIndex modelStack }
                                          else debugstate { debug = not debug, stackIndex = 0 }
                         StepForward -> debugstate { stackIndex = max 0 (stackIndex - 1) }
                         JumpForward -> debugstate { stackIndex = max 0 (stackIndex - 10) }
                         StepBackward -> debugstate { stackIndex = min (length modelStack - 1) (max 0 (stackIndex + 1)) }
                         JumpBackward -> debugstate { stackIndex = min (length modelStack - 1) (max 0 (stackIndex + 10)) }

debugLayer :: Layer b DebugMsg -> Layer (DebugState b, b) DebugMsg
debugLayer = mapState f . applyInput (splitInput debugInput) . wrap
    where f (debugstate@DebugState { debug, modelStack, stackIndex }, model) =
            if debug
                then (debugstate, modelStack !! stackIndex)
                else (debugstate { modelStack = model : (if length modelStack > 10000 then take 10000 modelStack else modelStack) }, model)

{-

reactTopLayer :: (s' -> s -> i -> s) ->
                 Layer (s, s') i ->
                 Layer (s, s') i
reactTopLayer inputf nextLayer (lay1, lay2) msgs =
        let (lay1', lay2') = nextLayer (lay1, lay2) msgs
            lay1'' = foldl' (inputf lay2') lay1' msgs in (lay1'', lay2')

react :: (s' -> s -> i -> s) ->
         (s' -> s -> s) ->
         Layer s' i ->
         Layer (s, s') i
react inputf stepf nextLayer (lay1, lay2) msgs =
        let lay2' = nextLayer lay2 msgs
            lay1' = foldl' (inputf lay2') lay1 msgs in
                (stepf lay2' lay1', lay2')

constructLayer :: (lay2 -> lay1 -> msg1 -> (lay1, [msg2])) ->
                  (lay2 -> lay1 -> lay1) ->
                  Layer lay2 msg2 ->
                  Layer (lay1, lay2) msg1
constructLayer inputf stepf nextLayer (lay1, lay2) msg1s =
        let (lay1', msgs) = mapAccumL (inputf lay2) lay1 msg1s
            lay2' = nextLayer lay2 (concat msgs)
            lay1'' = stepf lay2' lay1' in (lay1'', lay2')

constructStatelessLayer :: (lay2 -> msg1 -> [msg2]) -> Layer lay2 msg2 -> Layer lay2 msg1
constructStatelessLayer inputf nextLayer lay2 msg1s =
        nextLayer lay2 (concatMap (inputf lay2) msg1s)
        -}
