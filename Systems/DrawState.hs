{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}

module Systems.DrawState (make) where

import Engine.World
import Engine.System
import Engine.Component
import Math.Vector

data SysData = SysData deriving (Show)

make :: SysMonad c IO (System c)
make = return $ System run

run delta = 
      do
         updateComps2 (upDS delta) drawStates newtonianMovers

upDS delta (DrawState p) (NewtonianMover v a) =
      let delta' = realToFrac delta in
          (DrawState (p + v |* delta'))
