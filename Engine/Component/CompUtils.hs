{-# LANGUAGE Rank2Types #-}

module Engine.Component.CompUtils where

import Engine.Component.Entity
import Engine.Component.Model
import Control.Monad.State.Strict
import Control.Lens
import qualified Data.HashMap.Strict as HashMap

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

upComps2 cs target additional f = over target (\t -> stepComponentHash2 t (view additional cs) f) cs
          

