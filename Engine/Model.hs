{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module Engine.Model where

import Engine.Entity
import Control.Monad.State.Strict
import Control.Lens hiding (Context)
import Types.Types
import Camera.Camera
import Math.Matrix
import Graphics.Drawing

data Model cs gm = Model {
           _entities :: EntitySet,
           _components :: cs,
           _camera :: Camera,
           _game :: gm
           } deriving Show

newModel :: Camera -> cs -> gm -> Model cs gm
newModel cam cs gm = Model newEntitySet cs cam gm

runModel :: State (Model cs gm) a -> Model cs gm -> (a, Model cs gm)
runModel = runState

data RenderInfo = RenderInfo Mat44 (Size Int) Label

makeLenses ''Model

