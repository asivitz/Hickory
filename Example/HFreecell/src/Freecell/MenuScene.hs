module Freecell.MenuScene where

import Engine.Scene.Scene
import Engine.Scene.Input
import Engine.Component.Model
import Engine.Component.Component
import Freecell.Events
import Data.IORef
import Camera.Camera
import Types.Types
import Math.Matrix
import Graphics.Drawing
import Math.Vector
import Menus.Menus
import Systems.DrawText
import Systems.Draw

type Screen = MenuScreen Scalar InputEvent

data ComponentStore = ComponentStore

data Resources = Resources {
               pvcShader :: Shader,
               printer :: Printer Int
               }

type MenuModel = Model ComponentStore (TransitionStack Screen)

font = "goudy_bookletter_1911"

loadResources resPath = do
        shader <- loadShader resPath (fst pvcShaderPair) (snd pvcShaderPair)
        case shader of
            Nothing -> error "Couldn't load PVC Shader"
            Just sh -> do
                pr <- loadPrinter resPath sh font
                case pr of
                    Nothing -> error "Couldn't load printer"
                    Just p -> return $ Resources { printer = p, pvcShader = sh }

processInput :: RenderInfo -> InputEvent -> MenuModel -> (MenuModel, [InputEvent])
processInput ri ie model = (model, [])

stepModel :: Double -> MenuModel -> MenuModel
stepModel delta model@Model { _game = TransitionStack stk time leaving } = 
        model { _game = TransitionStack stk (time + delta) leaving }

render :: Resources -> RenderInfo -> MenuModel -> IO ()
render re ri model = return ()

makeScene = do
        is <- newIORef (Input [])
        let cam = Camera (Ortho 800 (-20) 1) (Route pZero Nothing)
            scene = Scene {
                          _name = "Menu",
                          _model = (newModel cam ComponentStore emptyTransitionStack),
                          _renderInfo = RenderInfo mat44Identity nullSize uiLabel,
                          _loadResources = loadResources "Example/HFreecell/resources",
                          _stepModel = makeStepModel processInput stepModel,
                          _render = render,
                          _inputStream = is,
                          _loadedRender = Nothing }
        return scene
