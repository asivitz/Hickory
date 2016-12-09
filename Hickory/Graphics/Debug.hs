module Hickory.Graphics.Debug
    (
    debugShape,
    debugShapeId,
    grabDebug,
    clearDebug,
    DebugShape(..),
    module Hickory.Color
    ) where

import System.IO.Unsafe
import Data.IORef
import Hickory.Math.Vector
import Hickory.Color

data DebugShape = DebugVector (V3 Scalar)
                | DebugPoint (V3 Scalar)
                | DebugLine (V3 Scalar) (V3 Scalar)

{-# NOINLINE lst #-}
lst :: IORef [(Color, DebugShape)]
lst = unsafePerformIO $ newIORef []

debugShape color s a = unsafePerformIO $ do
    modifyIORef lst ((color, s):)
    return a

debugShapeId color wrapper a = unsafePerformIO $ do
    modifyIORef lst ((color, wrapper a):)
    return a

grabDebug = readIORef lst

clearDebug =
        writeIORef lst []
