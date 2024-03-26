module Platforms.GLFW where

import Data.IORef (IORef)
import Hickory.Input
import Hickory.Types (Size)
import qualified Graphics.UI.GLFW as GLFW
import qualified Platforms.GLFW.Bridge as Bridge

getGLFWWindowSizeRef :: GLFW.Window -> IO (IORef (Size Int))
getGLFWWindowSizeRef = Bridge.getWindowSizeRef
