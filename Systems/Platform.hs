{-# LANGUAGE NamedFieldPuns #-}

module Systems.Platform where

import Engine.System
import Engine.Event

data SysData = SysData { 
             running :: Bool
            } deriving (Show)

empty = SysData { running = True }

quit' platform = putSysData platform SysData { running = False }

register platform rpc@RPC { quit = q } = rpc { quit = (quit' platform) : q }

make platform = System nullRun nullInit
