{-# LANGUAGE NamedFieldPuns #-}

module Utils.Projection where

import Math.VectorMatrix
import Types.Types
import Math.Vector
import Engine.Scene.Scene

unproject :: V2 -> Scalar -> RenderInfo -> V3
unproject pos z (RenderInfo mat ss _) = lerpUnproject pos z mat (viewportFromSize ss)
