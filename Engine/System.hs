{-# LANGUAGE Rank2Types #-}

module Engine.System where

import Engine.Component
import Engine.World
import Engine.Entity
import Control.Monad.State
import Data.IORef
import Utils.HashMap
import Control.Lens hiding (Context)

import qualified Data.HashMap.Strict as HashMap

data System c = System {
      runSys :: Double -> SysMonad c IO ()
      }

nullRun :: Double -> SysMonad c IO ()
nullRun _ = return ()

nullInit :: SysMonad c IO ()
nullInit = return ()

getWorld :: Monad m => SysMonad c m (World c)
getWorld = get

putWorld :: Monad m => World c -> SysMonad c m ()
putWorld = put

getRPC :: Monad m => SysMonad c m (RPC c)
getRPC = do
        World { _systemContext = (Context _ rpc) } <- get
        return rpc

putRPC :: Monad m => RPC c -> SysMonad c m ()
putRPC rpc = do
        w@World { _systemContext = Context cs _ } <- get
        put w { _systemContext = Context cs rpc }

registerRPC :: Monad m => (RPC c -> RPC c) -> SysMonad c m ()
registerRPC f = do
        rpc <- getRPC
        putRPC (f rpc)

registerResourceToWorld :: World c -> Lens' (RPC c) a -> a -> World c
registerResourceToWorld w@World { _systemContext = (Context cs rpc) } l f = 
        w { _systemContext = Context cs (set l f rpc) }

registerResource :: Monad m => Lens' (RPC c) a -> a -> SysMonad c m ()
registerResource l f = do
        w <- getWorld
        putWorld (registerResourceToWorld w l f)

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

removeEntities ents = do
        w <- get
        put $ deleteEntitiesFromWorld w ents

type CompStoreLens r cs rpc = Lens' (World r) (Context cs rpc)

type EntHash c = HashMap.HashMap Entity c

type CompLens cs c = Lens' cs (EntHash c)

putComponentStore :: Monad m => CompStoreLens c cs rpc -> cs -> SysMonad c m ()
putComponentStore l cs' = do
        w <- get
        {-w@World { systemContext = (Context _ rpc) } <- get-}
        put $ over l (\(Context _ rpc) -> (Context cs' rpc)) w
        {-put w { systemContext = Context cs' rpc }-}

getComponentStore :: Monad m => CompStoreLens c cs rpc -> SysMonad c m cs
getComponentStore l = do
        w <- get
        let (Context cs _) = view l w
        {-World { systemContext = (Context cs _) } <- get-}
        return cs

addComp :: (Monad m) => CompStoreLens r cs rpc -> Entity -> CompLens cs c -> c -> SysMonad r m ()
addComp l e comps c = do
        cs <- getComponentStore l

        let cs' = over comps (HashMap.insert e c) cs
        putComponentStore l cs'

deleteComponents :: (Monad m) => CompStoreLens r cs rpc -> CompLens cs c -> SysMonad r m ()
deleteComponents l comps = do
        cs <- getComponentStore l
        let cs' = set comps HashMap.empty cs
        putComponentStore l cs'

removeComp :: Monad m => CompStoreLens r cs rpc -> Entity -> CompLens cs c -> SysMonad r m ()
removeComp csl e comps = do
        cs <- getComponentStore csl

        let cs' = over comps (HashMap.delete e) cs
        putComponentStore csl cs'


compForEnt :: (Monad m) => CompStoreLens r cs rpc -> Entity -> CompLens cs c -> SysMonad r m (Maybe c)
compForEnt csl e complens = do
        cs <- getComponentStore csl
        let comps = view complens cs
            c = HashMap.lookup e comps
        return c

components :: Monad m => CompStoreLens r cs rpc -> CompLens cs c -> SysMonad r m (CompMap c)
components csl comps = do
        cs <- getComponentStore csl
        return $ view comps cs

componentsAsList :: Monad m => CompStoreLens r cs rpc -> CompLens cs c -> SysMonad r m [(Entity, c)]
componentsAsList csl comps = do
        cs <- getComponentStore csl
        return $ HashMap.toList $ view comps cs

getSysData :: IORef a -> SysMonad c IO a
getSysData a = liftIO $ readIORef a

-- This modifies the IORef strictly
putSysData :: IORef a -> a -> SysMonad c IO ()
putSysData a d = liftIO $ d `seq` writeIORef a d

getGameContext :: Monad m => SysMonad c m c
getGameContext = do
        World { _gameContext = gc } <- get
        return gc
        
putGameContext :: Monad m => c -> SysMonad c m ()
putGameContext gc = do
        w <- get
        put w { _gameContext = gc }

type WorldCompLens r c = Lens' (World r) (EntHash c)

zipComps2 :: World r -> WorldCompLens r c -> WorldCompLens r d -> [(Entity, c, d)]
zipComps2 w l m = zipHashes2 (view l w) (view m w)

zipComps3 :: World r -> WorldCompLens r c -> WorldCompLens r d -> WorldCompLens r e -> [(Entity, c, d, e)]
zipComps3 w l m n = zipHashes3 (view l w) (view m w) (view n w)

zipComps4 :: World r -> WorldCompLens r c -> WorldCompLens r d
                -> WorldCompLens r e -> WorldCompLens r f -> [(Entity, c, d, e, f)]
zipComps4 w l m n o = zipHashes4 (view l w) (view m w) (view n w) (view o w)

doComps2 :: Monad m => ((Entity, c, d) -> SysMonad r m ()) -> WorldCompLens r c -> WorldCompLens r d -> SysMonad r m ()
doComps2 f l m = do
        w <- getWorld
        mapM_ f (zipComps2 w l m)

upComps2 :: Monad m => (c -> d -> c) -> WorldCompLens r c -> WorldCompLens r d -> SysMonad r m ()
upComps2 f l m = do
        w <- getWorld
        let updated = map (\(e, c, d) -> (e, f c d)) (zipComps2 w l m)
        putWorld $ over l (HashMap.union (HashMap.fromList updated)) w

doComps3 :: Monad m => ((Entity, c, d, e) -> SysMonad r m ()) -> WorldCompLens r c 
    -> WorldCompLens r d -> WorldCompLens r e -> SysMonad r m ()
doComps3 f l m n = do
        w <- getWorld
        mapM_ f (zipComps3 w l m n)

doComps4 :: Monad m => ((Entity, c, d, e, f) -> SysMonad r m ()) -> WorldCompLens r c -> WorldCompLens r d -> 
    WorldCompLens r e -> WorldCompLens r f -> SysMonad r m ()
doComps4 f l m n o = do
        w <- getWorld
        mapM_ f (zipComps4 w l m n o)

sysComps :: CompLens ComponentStore c -> WorldCompLens r c
sysComps l = systemContext . compStore . l

gameComps :: CompLens cs c -> WorldCompLens (Context cs rpc) c
gameComps l = gameContext . compStore . l

updateCompsM :: Monad m => CompStoreLens r cs rpc -> (c -> SysMonad r m c) -> CompLens cs c -> SysMonad r m ()
updateCompsM csl f lp = do
        cs <- getComponentStore csl

        let kv_list = HashMap.toList (view lp cs)
            up1 orig@(e, c) = do
                r <- f c
                return (e, r)
        updated <- mapM up1 kv_list
        putComponentStore csl (set lp (HashMap.fromList updated) cs)

updateCompsM2 :: Monad m => CompStoreLens r cs rpc -> (c -> d -> SysMonad r m c) -> CompLens cs c -> CompLens cs d -> SysMonad r m ()
updateCompsM2 csl f lp ls = do
        cs <- getComponentStore csl

        let kv_list = HashMap.toList (view lp cs)
            additional = view ls cs
            up1 orig@(e, c) = let maybeadd = HashMap.lookup e additional in
                case maybeadd of
                    Nothing -> return orig
                    Just add -> do
                        r <- f c add
                        return (e, r)
        updated <- mapM up1 kv_list
        putComponentStore csl (set lp (HashMap.fromList updated) cs)

updateComps :: Monad m => CompStoreLens r cs rpc -> (c -> c) -> CompLens cs c -> SysMonad r m ()
updateComps csl f lp = do
        cs <- getComponentStore csl

        let kv_list = HashMap.toList (view lp cs)
            up1 orig@(e, c) = (e, f c)
            updated = map up1 kv_list
        putComponentStore csl (set lp (HashMap.fromList updated) cs)

updateComps2 :: Monad m => CompStoreLens r cs rpc -> (c -> d -> c) -> CompLens cs c -> CompLens cs d -> SysMonad r m ()
updateComps2 csl f lp ls = do
        cs <- getComponentStore csl

        let kv_list = HashMap.toList (view lp cs)
            additional = view ls cs
            up1 orig@(e, c) = let maybeadd = HashMap.lookup e additional in
                case maybeadd of
                    Nothing -> orig
                    Just add -> (e, f c add)
            updated = map up1 kv_list
        putComponentStore csl (set lp (HashMap.fromList updated) cs)

putComps :: Monad m => CompStoreLens r cs rpc -> CompLens cs c -> [(Entity, c)] -> SysMonad r m ()
putComps csl lens kvlist = do
        cs <- getComponentStore csl
        putComponentStore csl (set lens (HashMap.fromList kvlist) cs)

{-
zipComps2 :: Monad m => CompStoreLens r cs rpc -> CompLens cs c -> CompLens cs d -> SysMonad r m [(Entity, c, d)]
zipComps2 csl c1lens c2lens = do
        cs <- getComponentStore csl
        let c1 = view c1lens cs
            c2 = view c2lens cs
        return $ zipHashes2 c1 c2
        -}

orM :: (Monad m) => [m Bool] -> m Bool
orM []          = return False
orM (f:fs)      = f >>= (\x -> if x then return True else orM fs)

runInterruptableEvent :: Monad m => (a -> SysMonad c m Bool) -> Lens' (RPC c) [a] -> SysMonad c m ()
runInterruptableEvent f l = do
    rpc <- getRPC
    let evs = view l rpc
    orM $ reverse (map f evs)
    return ()
