{-# LANGUAGE NamedFieldPuns #-}
 
import Engine.Run
import Engine.Model
import Engine.Component
import Types.Types
import Math.Vector
import Systems.Draw
import qualified Systems.DrawState as DrawState
import Types.Color
import Utils.Utils
import Engine.Input
import Engine.CompUtils
import Math.VectorMatrix
import Control.Lens
import Control.Monad
import Camera.Camera
import Graphics.Drawing
import FreeCell
import Data.IORef

{-import qualified Freecell.Systems.Game as FCGame-}
{-import qualified Freecell.Systems.Menu as FCMenu-}
{-import qualified Freecell.Systems.Draw as FCDraw-}

data Resources = Resources {
               solidShader :: Maybe Shader
               }

data GameModel = GameModel {
               board :: Board
               }

data InputEvent = RawEvent RawInput
                | NewGame

loadResources :: String -> IO Resources
loadResources path = do
        solid <- loadShader path "Shader.vsh" "SolidColor.fsh"
        return $ Resources solid

render :: Resources -> Model ComponentStore GameModel -> IO ()
render (Resources solidShader) model = do
        {-print $ "Rendering model: " ++ (show model)-}

        whenMaybe solidShader $ \sh -> do
            let ds = getModelComponents drawStates model
            forM_ (stripEnts ds) $ \(DrawState pos) ->
                drawSpec pos uiLabel (SolidSquare (Size 50 50) white sh)

spawnThing pos = do
        e <- spawnEntity
        addComp e drawStates $ DrawState pos
        addComp e mouseDrags $ MouseDrag (v3 0 0 0)
        {-addComp components e newtonianMovers $ NewtonianMover (v3 40 16 0) (v3 0 0 0)-}
        return ()

processInput :: RenderInfo -> InputEvent -> Model ComponentStore GameModel -> Model ComponentStore GameModel
processInput (RenderInfo mat ss) (RawEvent (InputTouchDown pos pid)) model = runModel (spawnThing p') model
    where p' = lerpUnproject pos 5 mat (viewportFromSize ss)

processInput (RenderInfo mat ss) (RawEvent (InputTouchLoc pos pid)) model = 
        over components (\cs -> upComps2 cs drawStates mouseDrags (DrawState.snapToMouse p')) model
    where p' = lerpUnproject pos 5 mat (viewportFromSize ss)

processInput (RenderInfo mat ss) NewGame model = model
        {-mapM_ (\c -> spawnCard fcgame (v3 0 0 (-5)) c) (allCards board)-}

processInput _ _ model = model

stepComponents :: Double -> ComponentStore -> ComponentStore
stepComponents delta cs = upComps2 cs drawStates newtonianMovers (DrawState.upDS delta)

main :: IO ()
main = do
        board <- makeGame
        is <- newIORef (Input [])
        let cam = Camera (Ortho 800 (-20) 1) (Route pZero Nothing)
            scene = Scene {
                          _loadResources = loadResources "Example/HFreecell/resources",
                          _stepModel = makeStepModel processInput stepComponents,
                          _render = render,
                          _inputStream = is,
                          _loadedRender = Nothing }
                          
        glfwMain cam 
                 emptyComponentStore 
                 (GameModel board)
                 scene
                 RawEvent
