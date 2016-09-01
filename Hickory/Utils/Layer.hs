{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}

module Hickory.Utils.Layer where

import Hickory.Math.Vector
import Hickory.Input
import Data.List
import Control.Lens
import Text.PrettyPrint.GenericPretty
import Data.Maybe
import Control.Layer
import Control.Layer.Debug

stepOnly :: (s -> Scalar -> s) -> s -> RawInput -> s
stepOnly f s (Step delta) = f s delta
stepOnly f s _ = s

type MonadicLayer m s i = s -> i -> m s

constructMonadicLayer :: Monad m => (s' -> s -> m s) -> Layer s' i -> MonadicLayer m (s, s') i
constructMonadicLayer stepf nextLayer (lay1, lay2) msg1s = do
        let lay2' = nextLayer lay2 msg1s
        lay1' <- stepf lay2' lay1
        return (lay1', lay2')

mapStateMonadic :: Monad m => (s -> s -> m s) -> Layer s i -> MonadicLayer m s i
mapStateMonadic f layer s i = f s $ layer s i

instance Out model => Out (DebugState model)
