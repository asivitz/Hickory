module Freecell.MenuScene where

import Engine.Scene
import Engine.Model
import Engine.Input
import Engine.Component
import Freecell.Events
import Data.IORef
import Camera.Camera
import Types.Types
import Math.Matrix
import Graphics.Drawing
import Math.Vector

step :: RenderInfo -> Input InputEvent -> Double -> Model cs Bool -> (Model cs Bool, [InputEvent])
step _ _ _ m@Model { _game = True } = (m { _game = False }, [NewGame])
step _ _ _ m = (m, [])

render _ _ = return ()

makeScene = do
        is <- newIORef (Input [])
        let cam = Camera (Ortho 800 (-20) 1) (Route pZero Nothing)
            scene = Scene {
                          _name = "Menu",
                          _model = (newModel cam emptyComponentStore True),
                          _renderInfo = RenderInfo mat44Identity nullSize worldLabel,
                          _loadResources = return (),
                          _stepModel = step,
                          _render = render,
                          _inputStream = is,
                          _loadedRender = Nothing }
        return scene
