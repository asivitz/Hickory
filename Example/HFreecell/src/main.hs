{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
 
import Types.Types
import Math.Vector
import Math.Matrix
import Camera.Camera
import Graphics.Drawing
import qualified Graphics.UI.GLFW as GLFW
import Data.Bits
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB.GeometryShader4
import System.Random
import Platforms.GLFW
import Platforms.GLFW.Utils
import FreeCell
import Engine.Scene.Input
import Freecell.Game
import Freecell.Render

-- This function calculates a view matrix, used during rendering
calcCameraMatrix :: Size Int -> Model -> Mat44
calcCameraMatrix (Size w h) _ =
        let proj = Ortho 10 1 100 True
            camera = Camera proj (v3 5 3 0) in
                cameraMatrix camera (aspectRatio (Size w h))

data Environment = Environment {
                 win :: GLFW.Window,
                 timePoller :: IO Double,
                 inputPoller :: IO [RawInput]
                 }

gameMain :: GLFW.Window -> IO ()
gameMain win = do
            initRenderer
            glClearColor 0.125 0.125 0.125 1
            glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA
            glActiveTexture gl_TEXTURE0

            glEnable gl_PROGRAM_POINT_SIZE -- for OSX

            rgen <- getStdGen
            let mdl = makeGame rgen
                ui = mkUI mdl rgen

            resources <- loadResources "resources"
            inputPoller <- makeInputPoller win
            timePoller <- makeTimePoller

            let env = Environment { win, timePoller, inputPoller }

            loop (mdl, ui) env resources

loop :: (Model, UIState) -> Environment -> Resources -> IO ()
loop (mdl, ui) env resources = do
    delta <- timePoller env
    input <- inputPoller env
    scrSize <- uncurry Size <$> GLFW.getFramebufferSize (win env)

    let mat = calcCameraMatrix scrSize mdl

    glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)

    let viewInfo = (mat, scrSize)
        (ui', mdl') = uiLayer (delta, viewInfo) (ui, mdl) input
        rt = render resources viewInfo mdl' ui'

    renderTree worldLayer rt

    renderCommands mat worldLayer

    resetRenderer
    GLFW.swapBuffers (win env)

    loop (mdl', ui') env resources

main :: IO ()
main = withWindow 800 600 "Demo" gameMain
