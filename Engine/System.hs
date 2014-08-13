{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}

module Engine.System where

import Engine.Component
import Engine.World
import Engine.Entity
import Control.Monad.State
import Data.IORef
import Control.Lens

import qualified Data.HashMap.Strict as HashMap

data System c = System {
      runSys :: Double -> SysMonad c IO (),
      initSys :: SysMonad c IO ()
      }

nullRun :: Double -> SysMonad c IO ()
nullRun _ = return ()

nullInit :: SysMonad c IO ()
nullInit = return ()

getWorld :: Monad m => SysMonad c m (World c)
getWorld = do
      w <- get
      return w

getRPC :: Monad m => SysMonad c m (RPC c)
getRPC = do
        World { systemContext = (SystemContext _ rpc) } <- get
        return rpc

putRPC :: Monad m => (RPC c) -> SysMonad c m ()
putRPC rpc = do
        w@World { systemContext = (SystemContext cs _) } <- get
        put w { systemContext = (SystemContext cs rpc) }

registerRPC :: Monad m => (RPC c -> RPC c) -> SysMonad c m ()
registerRPC f = do
        rpc <- getRPC
        putRPC (f rpc)

registerResource :: Monad m => Lens' (RPC c) a -> a -> SysMonad c m ()
registerResource l f = do
        rpc <- getRPC
        putRPC (set l f rpc)

registerEvent :: Monad m => Lens' (RPC c) [a] -> a -> SysMonad c m ()
registerEvent l f = do
        rpc <- getRPC
        putRPC (over l (f:) rpc)

spawnEntity :: Monad m => SysMonad c m Entity
spawnEntity = do
      w <- get
      let (e, w') = addNewEntity w
      put w'
      return e

putComponentStore :: Monad m => ComponentStore -> SysMonad c m ()
putComponentStore cs' = do
      w@World { systemContext = (SystemContext _ rpc) } <- get
      put w { systemContext = (SystemContext cs' rpc) }

getComponentStore :: Monad m => SysMonad c m ComponentStore
getComponentStore = do
        World { systemContext = (SystemContext cs _) } <- get
        return cs

upComps :: (Component c, Monad m) => (c -> c) -> (ComponentStore -> HashMap.HashMap Entity c) -> SysMonad r m ()
upComps f g = do
        cs <- getComponentStore
        let new_ds = HashMap.map f (g cs)
            cs' = updateComponents cs new_ds
        putComponentStore cs'

doMap :: (Component c, Monad m) => (Entity -> c -> SysMonad r m c) -> (Entity, c) -> SysMonad r m (Entity, c)
doMap map_func (k,c) = do
      r <- map_func k c
      return (k,r)

upCompsM :: (Component c, Monad m) => (Entity -> c -> SysMonad r m c) -> (ComponentStore -> HashMap.HashMap Entity c) -> SysMonad r m ()
upCompsM map_func retrieve_func = do
        cs <- getComponentStore
        let kv_list = HashMap.toList (retrieve_func cs)
        new_kv <- mapM (doMap map_func) kv_list
        let new_kv_hashmap = HashMap.fromList new_kv

        let cs' = updateComponents cs new_kv_hashmap
        putComponentStore cs'

addComp :: (Component c, Monad m) => Entity -> c -> SysMonad r m ()
addComp e c = do
        cs <- getComponentStore
        let cs' = addComponent cs e c
        putComponentStore cs'

compForEnt :: (Monad m, Component c) => Entity -> SysMonad r m (Maybe c)
compForEnt e = do
        cs <- getComponentStore
        let comps = getComponents cs
            c = HashMap.lookup e comps
        return c

{-putSysData :: Monad m => sysdata -> SysMonad sysdata m ()-}
{-putSysData sd = do-}
      {-(w, es, _) <- get-}
      {-put (w, es, sd)-}

getSysData :: IORef a -> SysMonad c IO a
getSysData a = liftIO $ readIORef a

putSysData :: IORef a -> a -> SysMonad c IO ()
putSysData a d = liftIO $ writeIORef a d

getGameContext :: Monad m => SysMonad c m c
getGameContext = do
        World { gameContext = gc } <- get
        return gc
        
putGameContext :: Monad m => c -> SysMonad c m ()
putGameContext gc = do
        w <- get
        put w { gameContext = gc }
