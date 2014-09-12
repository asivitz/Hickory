{-# LANGUAGE NamedFieldPuns #-}
 
import Engine.Run
import Engine.Model
import Engine.Component.Component
import Types.Types
import Math.Vector
import Systems.Draw
import qualified Systems.DrawState as DrawState
import Types.Color
import Utils.Utils
import Engine.Scene.Input
import Engine.CompUtils
import Math.VectorMatrix
import Control.Lens
import Control.Monad
import Camera.Camera
import Graphics.Drawing

data Resources = Resources {
               solidShader :: Maybe Shader
               }

loadResources :: String -> IO Resources
loadResources path = do
        solid <- loadShader path "Shader.vsh" "SolidColor.fsh"
        return $ Resources solid

render :: Resources -> Model ComponentStore -> IO ()
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

processInput :: RenderInfo -> InputEv -> Model ComponentStore -> Model ComponentStore
processInput (RenderInfo mat ss) (InputTouchDown pos pid) model = runModel (spawnThing p') model
    where p' = lerpUnproject pos 5 mat (viewportFromSize ss)

processInput (RenderInfo mat ss) (InputTouchLoc pos pid) model = 
        over components (\cs -> upComps2 cs drawStates mouseDrags (DrawState.snapToMouse p')) model
    where p' = lerpUnproject pos 5 mat (viewportFromSize ss)

processInput _ _ model = model

stepComponents :: Double -> ComponentStore -> ComponentStore
stepComponents delta cs = upComps2 cs drawStates newtonianMovers (DrawState.upDS delta)

main :: IO ()
main = do
        let cam = Camera (Ortho 800 (-20) 1) (Route pZero Nothing)
        glfwMain cam 
                 emptyComponentStore 
                 (loadResources "Example/HFreecell/resources") 
                 processInput 
                 stepComponents 
                 render
