module Engine.World where

import Engine.Entity
import Engine.Component

data World = World {
           entitySet :: EntitySet,
           componentStore :: ComponentStore
           } deriving (Show)

emptyWorld :: World
emptyWorld = World { entitySet = newEntitySet, 
                 componentStore = emptyComponentStore
                 }

addNewEntity :: World -> (Entity, World)
addNewEntity w = let es = entitySet w
                     (ent, new_es) = genEntity es
                     in (ent, w { entitySet = new_es })
