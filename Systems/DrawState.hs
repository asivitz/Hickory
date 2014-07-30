{-# LANGUAGE MultiParamTypeClasses #-}

module Systems.DrawState (empty, make, SysData(..)) where

import Engine.Entity
import Engine.System
import Engine.Event
import Engine.Component
import Math.Vector

data SysData = SysData deriving (Show)

empty = SysData

make = System run nullHandleEvent nullInit

runDS :: Double -> Entity -> DrawState -> SysMonad IO DrawState
runDS deltad e ds@(DrawState (V2 x y)) = do
      nm <- compForEnt e
      let delta = realToFrac deltad
      {-liftIO $ print ds-}
      case nm of
         Just (NewtonianMover (V2 vx vy) (V2 ax ay)) -> return (DrawState (V2 (x + vx * delta) (y + vy * delta)))
         Nothing -> return ds

run delta = 
      do
         upCompsM (runDS delta) drawStates
