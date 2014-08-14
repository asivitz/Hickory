{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}

module Engine.System where

import Engine.Component
import Engine.World
import Engine.Entity
import Control.Monad.State
import Data.IORef
import Control.Lens hiding (Context)

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
        World { systemContext = (Context _ rpc) } <- get
        return rpc

putRPC :: Monad m => (RPC c) -> SysMonad c m ()
putRPC rpc = do
        w@World { systemContext = (Context cs _) } <- get
        put w { systemContext = (Context cs rpc) }

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
      w@World { systemContext = (Context _ rpc) } <- get
      put w { systemContext = (Context cs' rpc) }

getComponentStore :: Monad m => SysMonad c m ComponentStore
getComponentStore = do
        World { systemContext = (Context cs _) } <- get
        return cs

addComp :: (Monad m) => Entity -> CompLens c -> c -> SysMonad r m ()
addComp e comps c = do
        cs <- getComponentStore

        let cs' = over comps (\m -> HashMap.insert e c m) cs
        putComponentStore cs'

compForEnt :: (Monad m) => Entity -> CompLens c -> SysMonad r m (Maybe c)
compForEnt e l = do
        cs <- getComponentStore
        let comps = view l cs
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

type CompLens c = Lens' ComponentStore (HashMap.HashMap Entity c)

updateCompsM :: Monad m => (c -> SysMonad r m c) -> CompLens c -> SysMonad r m ()
updateCompsM f lp = do
        cs <- getComponentStore

        let kv_list = HashMap.toList (view lp cs)
            up1 orig@(e, c) = do
                r <- f c
                return (e, r)
        updated <- mapM up1 kv_list
        putComponentStore (set lp (HashMap.fromList updated) cs)

updateCompsM2 :: Monad m => (c -> d -> SysMonad r m c) -> CompLens c -> CompLens d -> SysMonad r m ()
updateCompsM2 f lp ls = do
        cs <- getComponentStore

        let kv_list = HashMap.toList (view lp cs)
            additional = view ls cs
            up1 orig@(e, c) = let maybeadd = HashMap.lookup e additional in
                case maybeadd of
                    Nothing -> return orig
                    Just add -> do
                        r <- f c add
                        return (e, r)
        updated <- mapM up1 kv_list
        putComponentStore (set lp (HashMap.fromList updated) cs)

updateComps :: Monad m => (c -> c) -> CompLens c -> SysMonad r m ()
updateComps f lp = do
        cs <- getComponentStore

        let kv_list = HashMap.toList (view lp cs)
            up1 orig@(e, c) = (e, f c)
            updated = map up1 kv_list
        putComponentStore (set lp (HashMap.fromList updated) cs)

updateComps2 :: Monad m => (c -> d -> c) -> CompLens c -> CompLens d -> SysMonad r m ()
updateComps2 f lp ls = do
        cs <- getComponentStore

        let kv_list = HashMap.toList (view lp cs)
            additional = view ls cs
            up1 orig@(e, c) = let maybeadd = HashMap.lookup e additional in
                case maybeadd of
                    Nothing -> orig
                    Just add -> (e, f c add)
            updated = map up1 kv_list
        putComponentStore (set lp (HashMap.fromList updated) cs)
