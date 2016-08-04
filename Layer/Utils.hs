module Layer.Utils where

import Math.Vector
import Engine.Scene.Input

stepOnly :: (s -> Scalar -> s) -> s -> RawInput -> s
stepOnly f s (Step delta) = f s delta
stepOnly f s _ = s

