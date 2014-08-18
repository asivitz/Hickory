{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NamedFieldPuns #-}

module Systems.DrawState (make) where

import Engine.World
import Engine.System
import Engine.Component
import Math.Vector
import Utils.Projection
import Control.Monad.State

data SysData = SysData deriving (Show)

make :: SysMonad c IO (System c)
make = do
        registerEvent inputTouchLoc (inputTouchLoc')
        return $ System run

run delta = 
      do
         updateComps2 (upDS delta) drawStates newtonianMovers

upDS delta (DrawState p) (NewtonianMover v a) =
      let delta' = realToFrac delta in
          (DrawState (p + v |* delta'))

inputTouchLoc' pos touchid = do
        ComponentStore { _mouseDrags } <- getComponentStore
        liftIO $ print _mouseDrags
        unproj <- doLerpUnproject pos (-5)
        updateComps2 (snapToMouse unproj) drawStates mouseDrags
        return False

snapToMouse pos _ (MouseDrag offset) = 
        DrawState (pos + offset)
