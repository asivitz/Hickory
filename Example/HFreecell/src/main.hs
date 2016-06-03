{-# LANGUAGE NamedFieldPuns #-}
 
import Engine.Scene.Run
import Engine.Scene.Scene
import Types.Types
import Freecell.Events
import Freecell.Component
import qualified Freecell.GameScene as GameScene
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

makeMsg :: RawInput -> Maybe Msg
makeMsg _ = Nothing

physics :: Double -> Model -> Model
physics delta model = model

-- This function calculates a view matrix, used during rendering
calcCameraMatrix :: Size Int -> Model -> Mat44
calcCameraMatrix (Size w h) model = 
        let proj = Ortho 10 1 100 True
            camera = Camera proj (v3 5 3 0) in
                cameraMatrix camera (aspectRatio (Size w h))

gameMain :: GLFW.Window -> Size Int -> IO ()
gameMain win scrSize = do 
            initRenderer
            glClearColor 0.125 0.125 0.125 1
            glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA
            glActiveTexture gl_TEXTURE0

            glEnable gl_PROGRAM_POINT_SIZE -- for OSX

            rgen <- getStdGen
            let game = makeGame rgen
                mdl = Model rgen game
                ui = mkUI mdl

            resources <- loadResources "resources"
            inputPoller <- makeInputPoller win
            timePoller <- makeTimePoller

            loop (mdl, ui) win timePoller inputPoller rgen scrSize resources

loop (mdl, ui) win timePoller inputPoller rgen scrSize resources = do
    delta <- timePoller
    input <- inputPoller

    let msgs = mapMaybe makeMsg input
        mat = calcCameraMatrix scrSize mdl

    glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)

    let (ui', msgs) = foldl' (\(u, msgs) i -> let (u', msgs') = uiInput mat scrSize mdl u i in (u', msgs ++ msgs')) (ui, []) input
        mdl' = foldl' update mdl msgs
        rt = render resources mat scrSize mdl' ui'

    renderTree worldLayer rt

    renderCommands mat worldLayer

    resetRenderer
    GLFW.swapBuffers win

    loop (mdl', ui') win timePoller inputPoller rgen scrSize resources

main :: IO ()
main = glfwMain' "Demo"
                  (Size 640 480)
                  gameMain
