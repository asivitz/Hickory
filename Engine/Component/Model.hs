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

forModel :: Model cs gm -> State (Model cs gm) a -> (Model cs gm, a)
forModel model f = swap $ runModel f model

instance SceneModel (Model cs gm) where
        calcCameraMatrix ar model = cameraMatrix (_camera model) ar

makeLenses ''Model

makeStepModel :: (RenderInfo -> ie -> Model cs gm -> (Model cs gm, [ie])) ->
    (Double -> cs -> cs) -> 
    RenderInfo -> Input ie -> Double -> Model cs gm -> (Model cs gm, [ie])
makeStepModel procInputF stepCompF ri Input { inputEvents } delta model = 
        let accum (m, oes) ie = let (m', oes') = procInputF ri ie m in (m', oes' ++ oes)
            (model', outputEvents) = foldl accum (model,[]) inputEvents 
            model'' = over components (\cs -> stepCompF delta cs) model'
            in (model'', outputEvents)
