module Platforms.GLFW where

import Data.IORef (IORef)
import Hickory.Input
import Hickory.Platform
import Hickory.Types (Size)
import qualified Graphics.UI.GLFW as GLFW
import qualified Platforms.GLFW.Bridge as Bridge

makeGLFWInputPoller :: GLFW.Window -> IORef (Size Int) -> IO (IO [RawInput])
makeGLFWInputPoller win fbSizeRef = makeInputPoller (Bridge.setupInput win fbSizeRef)


getGLFWBufferSizeRef :: GLFW.Window -> IO (IORef (Size Int))
getGLFWBufferSizeRef = Bridge.getBufferSizeRef
