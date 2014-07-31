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
runDS deltad e ds@(DrawState p) = do
      nm <- compForEnt e
      let delta = realToFrac deltad
      {-liftIO $ print ds-}
      case nm of
         Just (NewtonianMover v a) -> return (DrawState (p + v |* delta))
         Nothing -> return ds

run delta = 
      do
         upCompsM (runDS delta) drawStates
