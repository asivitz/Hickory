module Platforms.GLFW where

import qualified Graphics.UI.GLFW as GLFW
import Platforms.GLFW.Utils
import qualified Platforms.GLFW.Bridge as Bridge
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB.GeometryShader4
import Data.Bits
import Data.IORef
import Graphics.Drawing
import Engine.Scene.Input
import Types.Types
import Platform.Common

-- NEW

makeGLFWInputPoller :: GLFW.Window -> IO (IO [RawInput])
makeGLFWInputPoller win = makeInputPoller (Bridge.setupInput win)

glfwMain' :: String -> Size Int -> (GLFW.Window -> Size Int -> IO ()) -> IO ()
glfwMain' name (Size w h) callback =
    withWindow w h name $ \win -> do
        (width, height) <- GLFW.getFramebufferSize win
        let scrSize = Size width height

        callback win scrSize
