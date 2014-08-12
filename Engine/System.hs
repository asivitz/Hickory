{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}

module Engine.System where

import Engine.Component
import Engine.World
import Engine.Event
import Engine.Entity
import Control.Monad.State
import Data.IORef
import Control.Lens

import qualified Data.HashMap.Strict as HashMap

data System = System {
      runSys :: Double -> SysMonad IO (),
      initSys :: SysMonad IO ()
      }

nullRun :: Double -> SysMonad IO ()
nullRun _ = return ()

nullInit :: SysMonad IO ()
nullInit = return ()

getWorld :: Monad m => SysMonad m World
getWorld = do
      (w, _) <- get
      return w

getRPC :: Monad m => SysMonad m RPC
getRPC = do
        (_, SystemContext _ rpc) <- get
        return rpc

putRPC :: Monad m => RPC -> SysMonad m ()
putRPC rpc = do
        (w, SystemContext cs _) <- get
        put (w, SystemContext cs rpc)

registerRPC :: Monad m => (RPC -> RPC) -> SysMonad m ()
registerRPC f = do
        rpc <- getRPC
        putRPC (f rpc)


registerResource :: Monad m => Lens' RPC a -> a -> SysMonad m ()
registerResource l f = do
        rpc <- getRPC
        putRPC (set l f rpc)

registerEvent :: Monad m => Lens' RPC [a] -> a -> SysMonad m ()
registerEvent l f = do
        rpc <- getRPC
        putRPC (over l (f:) rpc)

spawnEntity :: Monad m => SysMonad m Entity
spawnEntity = do
      (w, sc) <- get
      let (e, w') = addNewEntity w
      put (w', sc)
      return e

putComponentStore :: Monad m => ComponentStore -> SysMonad m ()
putComponentStore cs' = do
      (w, SystemContext _ rpc) <- get
      put (w, SystemContext cs' rpc)

getComponentStore :: Monad m => SysMonad m ComponentStore
getComponentStore = do
        (_, SystemContext cs _) <- get
        return cs

upComps :: (Component c, Monad m) => (c -> c) -> (ComponentStore -> HashMap.HashMap Entity c) -> SysMonad m ()
upComps f g = do
        cs <- getComponentStore
        let new_ds = HashMap.map f (g cs)
            cs' = updateComponents cs new_ds
        putComponentStore cs'

doMap :: (Component c, Monad m) => (Entity -> c -> SysMonad m c) -> (Entity, c) -> SysMonad m (Entity, c)
doMap map_func (k,c) = do
      r <- map_func k c
      return (k,r)

upCompsM :: (Component c, Monad m) => (Entity -> c -> SysMonad m c) -> (ComponentStore -> HashMap.HashMap Entity c) -> SysMonad m ()
upCompsM map_func retrieve_func = do
        cs <- getComponentStore
        let kv_list = HashMap.toList (retrieve_func cs)
        new_kv <- mapM (doMap map_func) kv_list
        let new_kv_hashmap = HashMap.fromList new_kv

        let cs' = updateComponents cs new_kv_hashmap
        putComponentStore cs'

addComp :: (Component c, Monad m) => Entity -> c -> SysMonad m ()
addComp e c = do
        cs <- getComponentStore
        let cs' = addComponent cs e c
        putComponentStore cs'

compForEnt :: (Monad m, Component c) => Entity -> SysMonad m (Maybe c)
compForEnt e = do
        cs <- getComponentStore
        let comps = getComponents cs
            c = HashMap.lookup e comps
        return c

{-putSysData :: Monad m => sysdata -> SysMonad sysdata m ()-}
{-putSysData sd = do-}
      {-(w, es, _) <- get-}
      {-put (w, es, sd)-}

getSysData :: IORef a -> SysMonad IO a
getSysData a = liftIO $ readIORef a

putSysData :: IORef a -> a -> SysMonad IO ()
putSysData a d = liftIO $ writeIORef a d
