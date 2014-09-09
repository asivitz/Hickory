{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module Engine.Model where

import Engine.Entity
import Engine.Component
import Math.Vector
import Control.Monad.State
import Graphics.GLUtils
import Graphics.DrawText
import Graphics.Drawing
import Menus.Menus
import Control.Lens hiding (Context)
import Types.Types
import Camera.Camera
import Math.Matrix
import Platform.IPhone
import qualified Graphics.UI.GLFW as GLFW

data Model = Model {
           _entities :: EntitySet,
           _components :: ComponentStore,
           _camera :: Camera
           } deriving Show

newModel :: Camera -> Model
newModel cam = Model newEntitySet emptyComponentStore cam


data RenderInfo = RenderInfo Mat44 (Size Int)

makeLenses ''Model
