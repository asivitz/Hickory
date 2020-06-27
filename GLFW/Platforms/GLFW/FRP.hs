module Platforms.GLFW.FRP where

import Hickory.FRP (CoreEventGenerators, coreEventGenerators)
import Platforms.GLFW (getGLFWWindowSizeRef, makeGLFWInputPoller)
import qualified Graphics.UI.GLFW as GLFW

glfwCoreEventGenerators :: GLFW.Window -> IO (IO (), CoreEventGenerators)
glfwCoreEventGenerators win = do
  wSizeRef   <- getGLFWWindowSizeRef win
  inputPoller <- makeGLFWInputPoller win wSizeRef
  coreEventGenerators inputPoller wSizeRef

