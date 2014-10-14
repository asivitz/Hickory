{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module Engine.Component.Model where

import Engine.Scene.Scene
import Engine.Scene.Input
import Engine.Component.Entity
import Control.Monad.State.Strict
import Control.Lens hiding (Context)
import Camera.Camera
import Data.Tuple
import Types.Types

data Model cs gm = Model {
           _entities :: EntitySet,
           _components :: cs,
           _camera :: Size Int -> Camera,
           _game :: gm
           }

instance (Show cs, Show gm) => Show (Model cs gm) where
        show Model { _entities, _components, _camera, _game } = 
            "Model: " ++ (show _entities) ++ " " 
            ++ (show _components) ++ " "
            ++ (show _game)

newModel :: (Size Int -> Camera) -> cs -> gm -> Model cs gm
newModel cam cs gm = Model newEntitySet cs cam gm

runModel :: State (Model cs gm) a -> Model cs gm -> (a, Model cs gm)
runModel = runState

forModel :: Model cs gm -> State (Model cs gm) a -> (Model cs gm, a)
forModel model f = swap $ runModel f model

instance SceneModel (Model cs gm) where
        calcCameraMatrix size model = cameraMatrix ((_camera model) size) (aspectRatio size)
