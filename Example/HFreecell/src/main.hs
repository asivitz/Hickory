{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
 
import Engine.Scene.Run
import Engine.Scene.Scene
import Types.Types
import Math.VectorMatrix
import Math.Vector
import Math.Matrix
import Camera.Camera
import Graphics.DrawUtils
import Graphics.Drawing
import qualified Graphics.UI.GLFW as GLFW
import Data.Bits
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB.GeometryShader4
import System.Random
import Platforms.GLFW
import FreeCell
import Data.Maybe
import Data.List
import Engine.Scene.Input
import Data.Dynamic
import React.React
import Debug.Trace

import Freecell.Render

physics :: Double -> Model -> Model
physics delta model = model

-- This function calculates a view matrix, used during rendering
calcCameraMatrix :: Size Int -> Model -> Mat44
calcCameraMatrix (Size w h) model = 
        let proj = Ortho 10 1 100 True
            camera = Camera proj (v3 5 3 0) in
                cameraMatrix camera (aspectRatio (Size w h))

data Environment = Environment {
                 win :: GLFW.Window,
                 timePoller :: IO Double,
                 inputPoller :: IO [RawInput],
                 scrSize :: Size Int
                 }

gameMain :: GLFW.Window -> Size Int -> IO ()
gameMain win scrSize = do 
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

            let env = Environment { win, timePoller, inputPoller, scrSize }

            loop (mdl, ui) env resources

loop :: (Model, UIState) -> Environment -> Resources -> IO ()
loop (mdl, ui) env resources = do
    delta <- timePoller env
    input <- inputPoller env

    let mat = calcCameraMatrix (scrSize env) mdl

    glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)

    let viewInfo = (mat, scrSize env)
        {-(ui', msgs) = mapAccumL (uiInput viewInfo mdl) ui input-}
        {-mdl' = foldl' update mdl (concat msgs)-}
        {-ui'' = stepUI mdl' delta ui'-}
        {-rt = render resources viewInfo mdl' ui''-}

    let (ui', mdl') = layer1 (delta, viewInfo) (ui, mdl) input
        rt = render resources viewInfo mdl' ui'

    renderTree worldLayer rt

    renderCommands mat worldLayer

    resetRenderer
    GLFW.swapBuffers (win env)

    loop (mdl', ui') env resources

main :: IO ()
main = glfwMain' "Demo"
                  (Size 640 480)
                  gameMain
