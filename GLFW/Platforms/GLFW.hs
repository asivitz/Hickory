module Platforms.GLFW where

import qualified Graphics.UI.GLFW as GLFW
import qualified Platforms.GLFW.Bridge as Bridge
import Hickory.Input
import Hickory.Platform

makeGLFWInputPoller :: GLFW.Window -> IO (IO [RawInput])
makeGLFWInputPoller win = makeInputPoller (Bridge.setupInput win)
