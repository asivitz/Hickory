module Platforms.GLFW.FRP where

import Hickory.FRP (CoreEventGenerators, coreEventGenerators)
import Platforms.GLFW (getGLFWBufferSizeRef, makeGLFWInputPoller)
import qualified Graphics.UI.GLFW as GLFW

glfwCoreEventGenerators :: GLFW.Window -> IO (IO (), CoreEventGenerators)
glfwCoreEventGenerators win = do
  fbSizeRef   <- getGLFWBufferSizeRef win
  inputPoller <- makeGLFWInputPoller win fbSizeRef
  coreEventGenerators inputPoller fbSizeRef

