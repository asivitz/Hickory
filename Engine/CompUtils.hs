{-# LANGUAGE Rank2Types #-}

module Engine.CompUtils where

import Engine.Entity
import Engine.Model
import Control.Monad.State.Strict
import Control.Lens
import qualified Data.HashMap.Strict as HashMap

type ModelState r = State Model r

addNewEntity :: Model -> (Entity, Model)
addNewEntity w = let es = _entities w
                     (ent, new_es) = genEntity es
                     in (ent, w { _entities = new_es })

spawnEntity :: ModelState Entity
spawnEntity = do
      w <- get
      let (e, w') = addNewEntity w
      put w'
      return e

putComponentStore :: Lens' Model cs -> cs -> ModelState ()
putComponentStore l cs' = do
        w <- get
        put $ set l cs' w

getComponentStore :: Lens' Model cs -> ModelState cs
getComponentStore l = do
        w <- get
        let cs = view l w
        {-World { systemContext = (Context cs _) } <- get-}
        return cs

type EntHash c = HashMap.HashMap Entity c
type CompLens cs c = Lens' cs (EntHash c)

addComp :: Lens' Model cs -> Entity -> CompLens cs c -> c -> ModelState ()
addComp l e comps c = do
        cs <- getComponentStore l

        let cs' = over comps (HashMap.insert e c) cs
        putComponentStore l cs'

getModelComponents l model = view (components . l) model

stripEnts compmap = map snd (HashMap.toList compmap)

for = flip map

stepComponentHash2 first second f = HashMap.fromList $ for (HashMap.toList first) $ \(e, c1) ->
    case HashMap.lookup e second of
        Nothing -> (e, c1)
        Just c2 -> (e, f c1 c2)

