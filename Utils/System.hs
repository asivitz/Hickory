module Utils.System where

import Engine.System
import Data.IORef
import Engine.World
import Control.Monad.State


printSysData :: Show a => IORef a -> SysMonad c IO ()
printSysData sys = do
        sd <- getSysData sys
        liftIO $ print sd
