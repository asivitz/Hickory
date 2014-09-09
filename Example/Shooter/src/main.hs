{-# LANGUAGE NamedFieldPuns #-}
 
import Engine.Run
import Engine.Model
import Engine.Component
import Bootstrap.Bootstrap
import Graphics.GLFWUtils
import Graphics.Drawing
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB.GeometryShader4
import Data.Bits
import qualified Graphics.UI.GLFW as GLFW
import Types.Types
import Camera.Camera
import Math.Vector
import qualified Systems.Draw as Draw
import qualified Systems.GLFWPlatform as GLFWPlatform
import qualified Systems.DrawState as DrawState
import Types.Color
import Utils.Utils
import Engine.Input
import Control.Monad.State.Strict
import Engine.CompUtils
import Math.Matrix
import Math.VectorMatrix
import Data.List
import qualified Data.HashMap.Strict as HashMap

data Resources = Resources {
               solidShader :: Maybe Shader
               }

loadShader' resPath vert frag = do
   let prefix = resPath ++ "/Shaders/"
   let (vsp, fsp) = ( prefix ++ vert, prefix ++ frag)

   shader <- loadShader vsp fsp
   return shader
        

loadResources :: String -> IO Resources
loadResources path = do
        solid <- loadShader' path "Shader.vsh" "SolidColor.fsh"
        return $ Resources solid

render (Resources solidShader) model = do
        {-print $ "Rendering model: " ++ (show model)-}

        whenMaybe solidShader $ \sh -> do
            let ds = getModelComponents drawStates model
            forM_ (stripEnts ds) $ \(DrawState pos) ->
                Draw.drawSpec pos uiLabel (SolidSquare (Size 50 50) white sh)

spawnThing pos = do
        e <- spawnEntity
        addComp components e drawStates $ DrawState pos
        addComp components e newtonianMovers $ NewtonianMover (v3 40 16 0) (v3 0 0 0)
        return ()

runModel :: State Model () -> Model -> Model
runModel = execState

processInput :: RenderInfo -> InputEv -> Model -> Model
processInput (RenderInfo mat ss) (InputTouchDown pos pid) model = runModel (spawnThing p') model
    where p' = lerpUnproject pos 5 mat (viewportFromSize ss)
processInput _ _ model = model

for = flip map

stepNewtonianMovers delta nms dss = HashMap.fromList $ for (HashMap.toList dss) $ \(e, ds) ->
    case HashMap.lookup e nms of
        Nothing -> (e, ds)
        Just nm -> (e, DrawState.upDS delta ds nm)

stepComponents delta cs@ComponentStore { _drawStates, _newtonianMovers } = 
        cs { _drawStates = stepNewtonianMovers delta _newtonianMovers _drawStates }

stepModel :: Input -> RenderInfo -> Double -> Model -> Model
stepModel Input { inputEvents } ri delta model = let model' = foldr (processInput ri) model inputEvents 
                                                     model'' = case model' of
                                                                   m@Model { _components } -> m { _components = stepComponents delta _components }
                                                     in model''

calcMatrixFromModel :: Size Int -> Model -> Mat44
calcMatrixFromModel scrSize model = let ar = aspectRatio scrSize in
    cameraMatrix (_camera model) ar

glfwRender :: GLFW.Window -> (Model -> IO ()) -> Mat44 -> Model -> IO ()
glfwRender win renderFunc matrix model = do
        renderFunc model

        glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)

        renderCommands matrix uiLabel

        resetRenderer
        GLFW.swapBuffers win

makeStepFunc :: IO Input -> (Input -> RenderInfo -> Double -> Model -> Model) -> (RenderInfo -> Double -> Model -> IO Model)
makeStepFunc inputFunc stepFunc = \ri delta model -> do
    input <- inputFunc
    return $ stepFunc input ri delta model

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

              run (Size width height) calcMatrixFromModel (glfwRender win (render resources)) (makeStepFunc grabInputFunc stepModel) (newModel cam)
