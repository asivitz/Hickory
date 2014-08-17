{-# LANGUAGE NamedFieldPuns #-}

module Systems.GLFWPlatform where

import Engine.System
import Engine.World
import Data.IORef

data SysData = SysData { 
             isRunning :: Bool
            } deriving (Show)

empty = SysData { isRunning = True }

running' platform = do
        SysData { isRunning } <- readIORef platform
        return isRunning 

make platform = System nullRun (initS platform)

initS platform = do
        registerResource running (running' platform)
