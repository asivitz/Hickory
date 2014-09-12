module Freecell.GameScene (makeScene, InputEvent(..)) where

import FreeCell
import Freecell.Events
import Engine.Input
import Engine.Component
import Math.Matrix
import Math.VectorMatrix
import Engine.Model
import Engine.Scene
import Systems.Draw
import Engine.CompUtils
import Types.Color
import Types.Types
import Data.IORef
import Data.Tuple
import Graphics.Drawing
import Utils.Utils
import Control.Monad
import Control.Lens
import Camera.Camera
import qualified Systems.DrawState as DrawState
import Math.Vector
import Debug.Trace

data Resources = Resources {
               solidShader :: Maybe Shader
               }

data GameModel = GameModel {
               gameBoard :: Board
               }

loadResources :: String -> IO Resources
loadResources path = do
        solid <- loadShader path "Shader.vsh" "SolidColor.fsh"
        return $ Resources solid

render :: Resources -> Model ComponentStore GameModel -> IO ()
render (Resources solidSh) model = do
        {-print $ "Rendering model: " ++ (show model)-}

        whenMaybe solidSh $ \sh -> do
            let ds = getModelComponents drawStates model
            forM_ (stripEnts ds) $ \(DrawState pos) ->
                drawSpec pos uiLabel (SolidSquare (Size 50 50) white sh)

spawnThing pos = do
        e <- spawnEntity
        addComp e drawStates $ DrawState pos
        addComp e mouseDrags $ MouseDrag (v3 0 0 0)
        {-addComp components e newtonianMovers $ NewtonianMover (v3 40 16 0) (v3 0 0 0)-}
        return []

processInput :: RenderInfo -> InputEvent -> Model ComponentStore GameModel -> (Model ComponentStore GameModel, [InputEvent])
processInput (RenderInfo mat ss _) (RawEvent (InputTouchDown pos pid)) model = swap $ runModel (spawnThing p') model
    where p' = lerpUnproject pos 5 mat (viewportFromSize ss)

processInput (RenderInfo mat ss _) (RawEvent (InputTouchLoc pos pid)) model = 
        (over components (\cs -> upComps2 cs drawStates mouseDrags (DrawState.snapToMouse p')) model, [])
    where p' = lerpUnproject pos 5 mat (viewportFromSize ss)

processInput (RenderInfo mat ss _) NewGame model = trace "new game wee!" (model, [])
        {-mapM_ (\c -> spawnCard fcgame (v3 0 0 (-5)) c) (allCards board)-}

processInput _ _ model = (model, [])

stepComponents :: Double -> ComponentStore -> ComponentStore
stepComponents delta cs = upComps2 cs drawStates newtonianMovers (DrawState.upDS delta)

makeScene = do
        board <- makeGame
        is <- newIORef (Input [])
        let cam = Camera (Ortho 800 (-20) 1) (Route pZero Nothing)
            scene = Scene {
                          _name = "Game",
                          _model = (newModel cam emptyComponentStore (GameModel board)),
                          _renderInfo = RenderInfo mat44Identity nullSize uiLabel,
                          _loadResources = loadResources "Example/HFreecell/resources",
                          _stepModel = makeStepModel processInput stepComponents,
                          _render = render,
                          _inputStream = is,
                          _loadedRender = Nothing }
        return scene
