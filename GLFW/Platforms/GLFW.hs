module Platforms.GLFW where

import qualified Graphics.UI.GLFW as GLFW
import Platforms.GLFW.Utils
import qualified Platforms.GLFW.Bridge as Bridge
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB.GeometryShader4
import Data.Bits
import Data.IORef
import Graphics.Drawing
import Engine.Scene.Scene
import Engine.Scene.Run
import Engine.Scene.Input
import Types.Types
import Platform.Common

glfwRender :: GLFW.Window -> [SceneOperator ie] -> IO ()
glfwRender win operators = do
        glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)

        mapM_ _renderOp operators

        resetRenderer
        GLFW.swapBuffers win

mapAll :: [a -> b] -> a -> [b]
mapAll fs a = map (\f -> f a) fs

glfwMain :: Show ie => String -> Size Int -> [SceneOperator ie] -> (RawInput -> IO ()) -> IO ()
glfwMain name (Size w h) operators sendRawInput =
          withWindow w h name $ \win -> do
              initRenderer
              glClearColor 0.125 0.125 0.125 1
              glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA
              glActiveTexture gl_TEXTURE0

              glEnable gl_PROGRAM_POINT_SIZE -- for OSX

              (width, height) <- GLFW.getFramebufferSize win

              let scrSize = Size width height

              sequence_ $ mapAll (map _initRenderer operators) scrSize

              stepInp <- Bridge.setupInput win sendRawInput
              run stepInp
                  operators
                  (glfwRender win)

-- NEW

makeGLFWInputPoller :: GLFW.Window -> IO (IO [RawInput])
makeGLFWInputPoller win = makeInputPoller (Bridge.setupInput win)

glfwMain' :: String -> Size Int -> (GLFW.Window -> Size Int -> IO ()) -> IO ()
glfwMain' name (Size w h) callback =
    withWindow w h name $ \win -> do
        (width, height) <- GLFW.getFramebufferSize win
        let scrSize = Size width height

        callback win scrSize
