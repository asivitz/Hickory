{-# LANGUAGE Rank2Types #-}

module Engine.GameContext where

import Engine.System
import Engine.World
import Control.Lens

data GameContext compStore rpc = GameContext compStore rpc

getGameRPC :: Monad m => SysMonad (GameContext c r) m r
getGameRPC = do
        (GameContext _ rpc) <- getGameContext
        return rpc

putGameRPC :: Monad m => r -> SysMonad (GameContext c r) m ()
putGameRPC grpc = do
        (GameContext cs _) <- getGameContext
        putGameContext (GameContext cs grpc)

registerGameResource :: Monad m => Lens' r a -> a -> SysMonad (GameContext c r) m ()
registerGameResource l f = do
        rpc <- getGameRPC
        putGameRPC (set l f rpc)

registerGameEvent :: Monad m => Lens' r [a] -> a -> SysMonad (GameContext c r) m ()
registerGameEvent l f = do
        rpc <- getGameRPC
        putGameRPC (over l (f:) rpc)
