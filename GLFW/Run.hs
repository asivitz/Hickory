module GLFW.Run where

import qualified Graphics.UI.GLFW as GLFW
import GLFW.Utils
import qualified Systems.GLFWPlatform as GLFWPlatform
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB.GeometryShader4
import Data.Bits
import Graphics.Drawing
import Engine.Scene.Scene
import Engine.Run

glfwRender :: GLFW.Window -> [SceneOperator ie] -> IO ()
glfwRender win operators = do
        glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)

        mapM_ _renderOp operators

        resetRenderer
        GLFW.swapBuffers win

mapAll :: [a -> b] -> a -> [b]
mapAll fs a = map (\f -> f a) fs

glfwMain :: Show ie => Size Int -> [SceneOperator ie] -> SceneOperator ie -> (RawInput -> ie) -> IO ()
glfwMain (Size w h) operators keyOperator pkgRawInput = do 
          withWindow w h "MVC!" $ \win -> do
              initRenderer
              glClearColor 0.125 0.125 0.125 1
              glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA
              glActiveTexture gl_TEXTURE0
                
              glEnable gl_PROGRAM_POINT_SIZE -- for OSX

              (width, height) <- GLFW.getFramebufferSize win

              let scrSize = (Size width height)

              sequence_ $ mapAll (map _initRenderer operators) scrSize

              stepInp <- GLFWPlatform.setupInput win (\raw -> (_addEvent keyOperator (pkgRawInput raw)))
              run stepInp
                  operators
                  (glfwRender win)
