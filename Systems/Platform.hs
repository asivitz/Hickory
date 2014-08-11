{-# LANGUAGE NamedFieldPuns #-}

module Systems.Platform where

import Engine.System
import Engine.Event

data SysData = SysData { 
             running :: Bool
            } deriving (Show)

empty = SysData { running = True }

quit' platform = putSysData platform SysData { running = False }

make platform = System nullRun (initS platform)

initS platform = do
        rpc@RPC { quit = q } <- getRPC
        putRPC rpc { quit = (quit' platform) : q }
