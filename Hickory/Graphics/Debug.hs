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
                | DebugAngle Scalar

{-# NOINLINE lst #-}
lst :: IORef [(Color, String, DebugShape)]
lst = unsafePerformIO $ newIORef []

colors = [rgb 1 1 0,
          rgb 0 1 1,
          rgb 1 0 1,
          rgb 1 1 1,
          rgb 1 0.7 0.4,
          rgb 1 0.4 0.7,
          rgb 0.7 1 0.4,
          rgb 0.7 0.4 1,
          rgb 0.4 1 0.7,
          rgb 0.4 0.7 1
          ]

addShape :: (String, DebugShape) -> [(Color, String, DebugShape)] -> [(Color, String, DebugShape)]
addShape (label, shape) xs = if not $ any (\(_,l',_) -> label == l') xs then (colors !! length xs, label, shape) : xs else xs

debugShape label s a = unsafePerformIO $ do
    modifyIORef lst (addShape (label, s))
    return a

debugShapeId label wrapper a = unsafePerformIO $ do
    modifyIORef lst (addShape (label, wrapper a))
    return a

grabDebug = readIORef lst

clearDebug =
        writeIORef lst []
