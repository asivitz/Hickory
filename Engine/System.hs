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

getContext :: Monad m => ContextLens r cs rpc -> SysMonad r m (Context cs rpc)
getContext l = do
        w <- get
        return $ view l w
        
putContext :: Monad m => ContextLens r cs rpc -> (Context cs rpc) -> SysMonad r m ()
putContext l gc = do
        w <- get
        put $ set l gc w

getRPC :: Monad m => ContextLens r cs rpc -> SysMonad r m rpc
getRPC l = do
        w <- get
        let (Context _ rpc) = view l w
        return rpc

putRPC :: Monad m => ContextLens r cs rpc -> rpc -> SysMonad r m ()
putRPC l rpc = do
        w <- get
        let (Context cs _) = view l w
        put $ set l (Context cs rpc) w

registerResourceToWorld :: ContextLens r cs rpc -> World r -> Lens' rpc a -> a -> World r
registerResourceToWorld cl w l f = 
        let (Context cs rpc) = view cl w in
            set cl (Context cs (set l f rpc)) w

registerResource :: Monad m => ContextLens r cs rpc -> Lens' rpc a -> a -> SysMonad r m ()
registerResource cl l f = do
        w <- getWorld
        putWorld (registerResourceToWorld cl w l f)

registerEvent :: Monad m => ContextLens r cs rpc -> Lens' rpc [a] -> a -> SysMonad r m ()
registerEvent cl l f = do
        rpc <- getRPC cl
        putRPC cl (over l (f:) rpc)

spawnEntity :: Monad m => SysMonad c m Entity
spawnEntity = do
      w <- get
      let (e, w') = addNewEntity w
      put w'
      return e

removeEntities ents = do
        w <- get
        put $ deleteEntitiesFromWorld w ents

type ContextLens r cs rpc = Lens' (World r) (Context cs rpc)

type EntHash c = HashMap.HashMap Entity c

type CompLens cs c = Lens' cs (EntHash c)

putComponentStore :: Monad m => ContextLens c cs rpc -> cs -> SysMonad c m ()
putComponentStore l cs' = do
        w <- get
        {-w@World { systemContext = (Context _ rpc) } <- get-}
        put $ over l (\(Context _ rpc) -> (Context cs' rpc)) w
        {-put w { systemContext = Context cs' rpc }-}

getComponentStore :: Monad m => ContextLens c cs rpc -> SysMonad c m cs
getComponentStore l = do
        w <- get
        let (Context cs _) = view l w
        {-World { systemContext = (Context cs _) } <- get-}
        return cs

addComp :: (Monad m) => ContextLens r cs rpc -> Entity -> CompLens cs c -> c -> SysMonad r m ()
addComp l e comps c = do
        cs <- getComponentStore l

        let cs' = over comps (HashMap.insert e c) cs
        putComponentStore l cs'

deleteComponents :: (Monad m) => ContextLens r cs rpc -> CompLens cs c -> SysMonad r m ()
deleteComponents l comps = do
        cs <- getComponentStore l
        let cs' = set comps HashMap.empty cs
        putComponentStore l cs'

removeComp :: Monad m => ContextLens r cs rpc -> Entity -> CompLens cs c -> SysMonad r m ()
removeComp csl e comps = do
        cs <- getComponentStore csl

        let cs' = over comps (HashMap.delete e) cs
        putComponentStore csl cs'


compForEnt :: (Monad m) => ContextLens r cs rpc -> Entity -> CompLens cs c -> SysMonad r m (Maybe c)
compForEnt csl e complens = do
        cs <- getComponentStore csl
        let comps = view complens cs
            c = HashMap.lookup e comps
        return c

components :: Monad m => ContextLens r cs rpc -> CompLens cs c -> SysMonad r m (CompMap c)
components csl comps = do
        cs <- getComponentStore csl
        return $ view comps cs

componentsAsList :: Monad m => ContextLens r cs rpc -> CompLens cs c -> SysMonad r m [(Entity, c)]
componentsAsList csl comps = do
        cs <- getComponentStore csl
        return $ HashMap.toList $ view comps cs

getSysData :: IORef a -> SysMonad c IO a
getSysData a = liftIO $ readIORef a

-- This modifies the IORef strictly
putSysData :: IORef a -> a -> SysMonad c IO ()
putSysData a d = liftIO $ d `seq` writeIORef a d

type WorldCompLens r c = Lens' (World r) (EntHash c)

zipComps2w :: World r -> WorldCompLens r c -> WorldCompLens r d -> [(Entity, c, d)]
zipComps2w w l m = zipHashes2 (view l w) (view m w)

zipComps3w :: World r -> WorldCompLens r c -> WorldCompLens r d -> WorldCompLens r e -> [(Entity, c, d, e)]
zipComps3w w l m n = zipHashes3 (view l w) (view m w) (view n w)

zipComps4w :: World r -> WorldCompLens r c -> WorldCompLens r d
                -> WorldCompLens r e -> WorldCompLens r f -> [(Entity, c, d, e, f)]
zipComps4w w l m n o = zipHashes4 (view l w) (view m w) (view n w) (view o w)

zipComps2 :: Monad m => WorldCompLens r c -> WorldCompLens r d -> SysMonad r m [(Entity, c, d)]
zipComps2 l m = getWorld >>= (\w -> return $ zipComps2w w l m)

zipComps3 :: Monad m => WorldCompLens r c -> WorldCompLens r d -> WorldCompLens r e -> SysMonad r m [(Entity, c, d, e)]
zipComps3 l m n = getWorld >>= (\w -> return $ zipComps3w w l m n)

zipComps4 :: Monad m => WorldCompLens r c -> WorldCompLens r d
                -> WorldCompLens r e -> WorldCompLens r f -> SysMonad r m [(Entity, c, d, e, f)]
zipComps4 l m n o = getWorld >>= (\w -> return $ zipComps4w w l m n o)

doComps2 :: Monad m => ((Entity, c, d) -> SysMonad r m ()) -> WorldCompLens r c -> WorldCompLens r d -> SysMonad r m ()
doComps2 f l m = do
        zipComps2 l m >>= mapM_ f 

upComps2 :: Monad m => (c -> d -> c) -> WorldCompLens r c -> WorldCompLens r d -> SysMonad r m ()
upComps2 f l m = do
        w <- getWorld
        let updated = map (\(e, c, d) -> (e, f c d)) (zipComps2w w l m)
        putWorld $ over l (HashMap.union (HashMap.fromList updated)) w

doComps3 :: Monad m => ((Entity, c, d, e) -> SysMonad r m ()) -> WorldCompLens r c 
    -> WorldCompLens r d -> WorldCompLens r e -> SysMonad r m ()
doComps3 f l m n = do
        zipComps3 l m n >>= mapM_ f 

doComps4 :: Monad m => ((Entity, c, d, e, f) -> SysMonad r m ()) -> WorldCompLens r c -> WorldCompLens r d -> 
    WorldCompLens r e -> WorldCompLens r f -> SysMonad r m ()
doComps4 f l m n o = do
        zipComps4 l m n o >>= mapM_ f 

sysComps :: CompLens ComponentStore c -> WorldCompLens r c
sysComps l = systemContext . compStore . l

gameComps :: CompLens cs c -> WorldCompLens (Context cs rpc) c
gameComps l = gameContext . compStore . l

putComps :: Monad m => ContextLens r cs rpc -> CompLens cs c -> [(Entity, c)] -> SysMonad r m ()
putComps csl lens kvlist = do
        cs <- getComponentStore csl
        putComponentStore csl (set lens (HashMap.fromList kvlist) cs)

orM :: (Monad m) => [m Bool] -> m Bool
orM []          = return False
orM (f:fs)      = f >>= (\x -> if x then return True else orM fs)

runInterruptableEvent :: Monad m => ContextLens r cs rpc -> (a -> SysMonad r m Bool) -> Lens' rpc [a] -> SysMonad r m ()
runInterruptableEvent cl f l = do
    rpc <- getRPC cl
    let evs = view l rpc
    orM $ reverse (map f evs)
    return ()
