module Engine.World where

import Engine.Entity
import Engine.Component

data World = World {
           entitySet :: EntitySet
           } deriving (Show)

emptyWorld :: World
emptyWorld = World { entitySet = newEntitySet
                 }

addNewEntity :: World -> (Entity, World)
addNewEntity w = let es = entitySet w
                     (ent, new_es) = genEntity es
                     in (ent, w { entitySet = new_es })
