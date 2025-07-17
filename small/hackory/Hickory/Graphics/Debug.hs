module Hickory.Graphics.Debug
    (
    DebugShape(..)
    ) where

import System.IO.Unsafe
import Data.IORef
import Hickory.Math.Vector
import Linear (V3)

data DebugShape = DebugVector (V3 Scalar)
                | DebugPoint (V3 Scalar)
                | DebugLine (V3 Scalar) (V3 Scalar)
                | DebugAngle Scalar

