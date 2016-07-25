{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveGeneric #-}

module Layer.Layer where

import Data.List
import Control.Lens
import Text.PrettyPrint.GenericPretty

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

mapStateMonadic :: Monad m => (s -> s -> m s) -> Layer s i -> MonadicLayer m s i
mapStateMonadic f layer s i = f s $ layer s i

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
             deriving (Show, Generic)

-- GENERATED #################
-- makeLenses ''DebugState
current :: forall model_aI9. Lens' (DebugState model_aI9) model_aI9
current f_a6OS (DebugState x_a6OT x_a6OU x_a6OV x_a6OW)
    = fmap
        (\ y_a6OX -> DebugState x_a6OT x_a6OU x_a6OV y_a6OX)
        (f_a6OS x_a6OW)
{-# INLINE current #-}
debug :: forall model_aI9. Lens' (DebugState model_aI9) Bool
debug f_a6OY (DebugState x_a6OZ x_a6P0 x_a6P1 x_a6P2)
    = fmap
        (\ y_a6P3 -> DebugState y_a6P3 x_a6P0 x_a6P1 x_a6P2)
        (f_a6OY x_a6OZ)
{-# INLINE debug #-}
modelStack ::
    forall model_aI9. Lens' (DebugState model_aI9) [model_aI9]
modelStack f_a6P4 (DebugState x_a6P5 x_a6P6 x_a6P7 x_a6P8)
    = fmap
        (\ y_a6P9 -> DebugState x_a6P5 y_a6P9 x_a6P7 x_a6P8)
        (f_a6P4 x_a6P6)
{-# INLINE modelStack #-}
stackIndex :: forall model_aI9. Lens' (DebugState model_aI9) Int
stackIndex f_a6Pa (DebugState x_a6Pb x_a6Pc x_a6Pd x_a6Pe)
    = fmap
        (\ y_a6Pf -> DebugState x_a6Pb x_a6Pc y_a6Pf x_a6Pe)
        (f_a6Pa x_a6Pd)
{-# INLINE stackIndex #-}
-- ##########################

instance Out model => Out (DebugState model)

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
            else debugstate & modelStack .~ _current : (if length _modelStack > 1500 then take 1500 _modelStack else _modelStack)

debugLayer :: (i -> [DebugMsg]) -> LayerXForm b (DebugState b) i
debugLayer debugMsgF = mapState debugStep . applyInput (transformInput (\s i -> debugMsgF i) debugInput) . wrap current
