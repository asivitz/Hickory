{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module Engine.Model where

import Engine.Entity
import Engine.Component
import Control.Monad.State.Strict
import Control.Lens hiding (Context)
import Types.Types
import Camera.Camera
import Math.Matrix

data Model = Model {
           _entities :: EntitySet,
           _components :: ComponentStore,
           _camera :: Camera
           } deriving Show

newModel :: Camera -> Model
newModel cam = Model newEntitySet emptyComponentStore cam

runModel :: State Model () -> Model -> Model
runModel = execState

data RenderInfo = RenderInfo Mat44 (Size Int)

makeLenses ''Model

