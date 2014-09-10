{-# LANGUAGE NamedFieldPuns #-}
 
import Engine.Run
import Engine.Model
import Engine.Component
import Graphics.GLFWUtils
import Graphics.Drawing
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB.GeometryShader4
import Data.Bits
import qualified Graphics.UI.GLFW as GLFW
import Types.Types
import Camera.Camera
import Math.Vector
import Systems.Draw
import qualified Systems.GLFWPlatform as GLFWPlatform
import qualified Systems.DrawState as DrawState
import Types.Color
import Utils.Utils
import Engine.Input
import Engine.CompUtils
import Math.Matrix
import Math.VectorMatrix
import Control.Lens
import Control.Monad

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

stepModel :: (RenderInfo -> InputEv -> Model cs -> Model cs) ->
    (Double -> cs -> cs) -> 
    Input -> RenderInfo -> Double -> Model cs -> Model cs
stepModel procInputF stepCompF Input { inputEvents } ri delta model = let model' = foldr (procInputF ri) model inputEvents 
                                                                          model'' = over components (\cs -> stepCompF delta cs) model'
                                                                          in model''

calcMatrixFromModel :: Size Int -> Model cs -> Mat44
calcMatrixFromModel scrSize model = let ar = aspectRatio scrSize in
    cameraMatrix (_camera model) ar

glfwRender :: GLFW.Window -> (Model cs -> IO ()) -> Mat44 -> Model cs -> IO ()
glfwRender win renderFunc matrix model = do
        renderFunc model

        glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)

        renderCommands matrix uiLabel

        resetRenderer
        GLFW.swapBuffers win

main :: IO ()
main = do 
          withWindow 640 480 "MVC!" $ \win -> do
              initRenderer
              glClearColor 0.3 0.5 0 1
              glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA
              glActiveTexture gl_TEXTURE0
                
              glEnable gl_PROGRAM_POINT_SIZE -- for OSX

              (width, height) <- GLFW.getFramebufferSize win

              resources <- loadResources "Example/HFreecell/resources"
              let cam = Camera (Ortho 800 (-20) 1) (Route pZero Nothing)

              grabInputFunc <- GLFWPlatform.makeGrabInput win

              run (Size width height) 
                  calcMatrixFromModel 
                  (glfwRender win (render resources)) 
                  (makeStepFunc grabInputFunc (stepModel processInput stepComponents))
                  (newModel cam emptyComponentStore)
