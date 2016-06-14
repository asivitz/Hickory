{-# LANGUAGE NamedFieldPuns #-}

module Layer.Layer where

import Data.List

type Layer s i = s -> [i] -> s

constructLayer :: (lay2 -> lay1 -> msg1 -> (lay1, [msg2])) ->
                  (lay2 -> lay1 -> lay1) ->
                  Layer lay2 msg2 ->
                  Layer (lay1, lay2) msg1
constructLayer inputf stepf nextLayer (lay1, lay2) msg1s =
        let (lay1', msgs) = mapAccumL (inputf lay2) lay1 msg1s
            lay2' = nextLayer lay2 (concat msgs)
            lay1'' = stepf lay2' lay1' in (lay1'', lay2')

constructStatelessLayer :: (lay2 -> msg1 -> [msg2]) -> Layer lay2 msg2 -> Layer (lay1, lay2) msg1
constructStatelessLayer inputf =
        constructLayer (\lay2 ui msg1 -> (ui, inputf lay2 msg1)) (const id)

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

debugLayer :: Layer b msg -> Layer (DebugState b, b) DebugMsg
debugLayer subLayer (debugstate@DebugState { debug, modelStack, stackIndex }, model) debugmsgs = ret
        where uis' = foldl' (debugInput model) debugstate debugmsgs
              mdl' = subLayer model []
              ret = if debug
                          then (uis', modelStack !! stackIndex)
                          else (uis' { modelStack = mdl' : (if length modelStack > 10000 then take 10000 modelStack else modelStack) }, mdl')
