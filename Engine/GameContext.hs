{-# LANGUAGE Rank2Types #-}

module Engine.GameContext where

import Engine.System
import Engine.World
import Control.Lens hiding (Context)

getGameRPC :: Monad m => SysMonad (Context c r) m r
getGameRPC = do
        (Context _ rpc) <- getGameContext
        return rpc

putGameRPC :: Monad m => r -> SysMonad (Context c r) m ()
putGameRPC grpc = do
        (Context cs _) <- getGameContext
        putGameContext (Context cs grpc)

registerGameResource :: Monad m => Lens' r a -> a -> SysMonad (Context c r) m ()
registerGameResource l f = do
        rpc <- getGameRPC
        putGameRPC (set l f rpc)

registerGameEvent :: Monad m => Lens' r [a] -> a -> SysMonad (Context c r) m ()
registerGameEvent l f = do
        rpc <- getGameRPC
        putGameRPC (over l (f:) rpc)
