{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NamedFieldPuns #-}

module Systems.DrawState (upDS, snapToMouse) where

import Engine.Component.Component
import Math.Vector


upDS delta (DrawState p) (NewtonianMover v a) =
      let delta' = realToFrac delta in
          (DrawState (p + v |* delta'))

snapToMouse pos _ (MouseDrag offset) = 
        DrawState (pos + offset)
