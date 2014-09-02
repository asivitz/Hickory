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

{-printAll' = do-}
        {-dss <- (components drawStates)-}
        {-liftIO $ print dss-}

make :: SysMonad c IO (System c)
make = do
        registerEvent inputTouchLoc (inputTouchLoc')
        {-registerEvent printAll (printAll')-}
        return $ System run

run delta = 
      do
         upComps2 (upDS delta) (sysComps drawStates) (sysComps newtonianMovers)

upDS delta (DrawState p) (NewtonianMover v a) =
      let delta' = realToFrac delta in
          (DrawState (p + v |* delta'))

inputTouchLoc' pos touchid = do
        unproj <- doLerpUnproject pos (-5)
        upComps2 (snapToMouse unproj) (sysComps drawStates) (sysComps mouseDrags)
        return False

snapToMouse pos _ (MouseDrag offset) = 
        DrawState (pos + offset)
