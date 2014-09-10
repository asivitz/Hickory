{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module Engine.Model where

import Engine.Entity
import Control.Monad.State.Strict
import Control.Lens hiding (Context)
import Types.Types
import Camera.Camera
import Math.Matrix

data Model cs = Model {
           _entities :: EntitySet,
           _components :: cs,
           _camera :: Camera
           } deriving Show

newModel :: Camera -> cs -> Model cs
newModel cam cs = Model newEntitySet cs cam

runModel :: State (Model cs) () -> Model cs -> Model cs
runModel = execState

data RenderInfo = RenderInfo Mat44 (Size Int)

makeLenses ''Model

