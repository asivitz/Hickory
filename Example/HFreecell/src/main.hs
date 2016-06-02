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

            resources <- loadResources "resources"
            inputPoller <- makeInputPoller win
            timePoller <- makeTimePoller

            loop (Model rgen game)  win timePoller inputPoller rgen scrSize resources (mkTerminal NoRender)

loop mdl win timePoller inputPoller rgen scrSize resources oldcomp = do
    delta <- timePoller
    input <- inputPoller

    let msgs = mapMaybe makeMsg input
        mat = calcCameraMatrix scrSize mdl

    glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)


    let comp = view resources mat scrSize mdl
        comp' = resolveComponents oldcomp comp
        comp'' = stepComp $ foldl' (\c i -> inputComp i c) comp' input

    render worldLayer comp''

    renderCommands mat worldLayer

    resetRenderer
    GLFW.swapBuffers win

    let mdl' = physics delta $ foldl' (flip GameScene.update) mdl msgs

    loop mdl' win timePoller inputPoller rgen scrSize resources comp''

main :: IO ()
main = glfwMain' "Demo"
                  (Size 640 480)
                  gameMain
