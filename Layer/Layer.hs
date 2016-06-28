{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module Layer.Layer where

import Data.List
import Lens.Micro
import Lens.Micro.TH

type Layer s i = s -> [i] -> s

type LayerXForm s' s i = Layer s' i -> Layer s i

type MonadicLayer m s i = s -> [i] -> m s

constructMonadicLayer :: Monad m => (s' -> s -> m s) -> Layer s' i -> MonadicLayer m (s, s') i
constructMonadicLayer stepf nextLayer (lay1, lay2) msg1s = do
        let lay2' = nextLayer lay2 msg1s
        lay1' <- stepf lay2' lay1
        return (lay1', lay2')

wrap :: Lens' s s' -> Layer s' i -> Layer s i
wrap l nextLayer s i = s & l %~ (`nextLayer` i)

noXForm s i = [i]

applyInput :: (s -> i -> s) -> Layer s i -> Layer s i
applyInput inputf nextLayer s msgs = foldl' inputf next msgs
    where next = nextLayer s msgs

mapState :: (s -> s -> s) -> Layer s i -> Layer s i
mapState f layer s i = f s $ layer s i

transformInput :: (s -> i -> [j]) -> (s -> j -> s) -> (s -> i -> s)
transformInput xformer inputf s i = foldl' inputf s (xformer s i)

-- Debug Layer
data DebugMsg = FlipDebug
              | StepForward
              | JumpForward
              | StepBackward
              | JumpBackward

data DebugState model = DebugState {
             _debug :: Bool,
             _modelStack :: [model],
             _stackIndex :: Int,
             _current :: model
             }
             deriving (Show)

makeLenses ''DebugState

mkDebugState = DebugState False [] 0

debugInput :: DebugState b -> DebugMsg -> DebugState b
debugInput debugstate@DebugState { _debug, _stackIndex, _modelStack } input = state'
        where state' = case input of
                         FlipDebug -> if _debug
                                          then debugstate { _debug = not _debug, _stackIndex = 0, _modelStack = drop _stackIndex _modelStack }
                                          else debugstate { _debug = not _debug, _stackIndex = 0 }
                         StepForward -> debugstate { _stackIndex = max 0 (_stackIndex - 1) }
                         JumpForward -> debugstate { _stackIndex = max 0 (_stackIndex - 10) }
                         StepBackward -> debugstate { _stackIndex = min (length _modelStack - 1) (max 0 (_stackIndex + 1)) }
                         JumpBackward -> debugstate { _stackIndex = min (length _modelStack - 1) (max 0 (_stackIndex + 10)) }

debugStep :: DebugState b -> DebugState b -> DebugState b
debugStep oldstate debugstate@DebugState { _debug, _modelStack, _stackIndex, _current } =
        if _debug
            then debugstate & current .~ _modelStack !! _stackIndex
            else debugstate & modelStack .~ _current : (if length _modelStack > 10000 then take 10000 _modelStack else _modelStack)

debugLayer :: (i -> [DebugMsg]) -> LayerXForm b (DebugState b) i
debugLayer debugMsgF = mapState debugStep . applyInput (transformInput (\s i -> debugMsgF i) debugInput) . wrap current
