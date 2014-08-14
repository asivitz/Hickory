{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}

module Systems.DrawState (empty, make, SysData(..)) where

import Engine.System
import Engine.Component
import Math.Vector

data SysData = SysData deriving (Show)

empty = SysData

make = System run nullInit

run delta = 
      do
         updateComps2 (upDS delta) drawStates newtonianMovers

upDS delta (DrawState p) (NewtonianMover v a) =
      let delta' = realToFrac delta in
          (DrawState (p + v |* delta'))
