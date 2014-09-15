{-# LANGUAGE Rank2Types #-}

module Engine.Component.CompUtils where

import Engine.Component.Entity
import Engine.Component.Model
import Control.Monad.State.Strict
import Control.Lens
import qualified Data.HashMap.Strict as HashMap
import Utils.HashMap

type ModelState cs gm r = State (Model cs gm) r

addNewEntity :: Model cs gm -> (Entity, Model cs gm)
addNewEntity w = let es = _entities w
                     (ent, new_es) = genEntity es
                     in (ent, w { _entities = new_es })

spawnEntity :: ModelState cs gm Entity
spawnEntity = do
      w <- get
      let (e, w') = addNewEntity w
      put w'
      return e

putComponentStore :: cs -> ModelState cs gm ()
putComponentStore cs' = do
        w <- get
        put $ set components cs' w

getComponentStore :: ModelState cs gm cs
getComponentStore = do
        w <- get
        let cs = view components w
        {-World { systemContext = (Context cs _) } <- get-}
        return cs

type EntHash c = HashMap.HashMap Entity c
type CompLens cs c = Lens' cs (EntHash c)

addComp :: Entity -> CompLens cs c -> c -> ModelState cs gm ()
addComp e comps c = do
        cs <- getComponentStore

        let cs' = over comps (HashMap.insert e c) cs
        putComponentStore cs'

getModelComponents l model = view (components . l) model

stripEnts compmap = map snd (HashMap.toList compmap)

for = flip map

stepComponentHash2 :: EntHash c -> EntHash d -> (c -> d -> c) -> EntHash c
stepComponentHash2 first second f = HashMap.fromList $ for (HashMap.toList first) $ \(e, c1) ->
    case HashMap.lookup e second of
        Nothing -> (e, c1)
        Just c2 -> (e, f c1 c2)

upComps2 :: cs -> CompLens cs c -> CompLens cs d -> (c -> d -> c) -> cs
upComps2 cs target additional f = over target (\t -> stepComponentHash2 t (view additional cs) f) cs
          
compForEnt :: Entity -> CompLens cs c -> ModelState cs gm (Maybe c)
compForEnt e complens = do
        cs <- getComponentStore
        let comps = view complens cs
            c = HashMap.lookup e comps
        return c

modelCompForEnt :: Model cs gm -> Entity -> CompLens cs c -> (Maybe c)
modelCompForEnt model ent l = let comps = view (components . l) model in
    HashMap.lookup ent comps

stepComponentHash2Ent :: EntHash c -> EntHash d -> (Entity -> c -> d -> c) -> EntHash c
stepComponentHash2Ent first second f = HashMap.fromList $ for (HashMap.toList first) $ \(e, c1) ->
    case HashMap.lookup e second of
        Nothing -> (e, c1)
        Just c2 -> (e, f e c1 c2)

upComps2Ent :: cs -> CompLens cs c -> CompLens cs d -> (Entity -> c -> d -> c) -> cs
upComps2Ent cs target additional f = over target (\t -> stepComponentHash2Ent t (view additional cs) f) cs

zipComps2 :: Model cs gm -> CompLens cs c -> CompLens cs d -> [(Entity, c, d)]
zipComps2 w l m = zipHashes2 (view l cs) (view m cs)
    where cs = _components w

zipComps3 :: Model cs gm -> CompLens cs c -> CompLens cs d -> CompLens cs e -> [(Entity, c, d, e)]
zipComps3 w l m n = zipHashes3 (view l cs) (view m cs) (view n cs)
    where cs = _components w

zipComps4 :: Model cs gm -> CompLens cs c -> CompLens cs d
                -> CompLens cs e -> CompLens cs f -> [(Entity, c, d, e, f)]
zipComps4 w l m n o = zipHashes4 (view l cs) (view m cs) (view n cs) (view o cs)
    where cs = _components w
