module Platforms.GLFW.FRP where

import Hickory.FRP.CoreEvents (CoreEventGenerators, coreEventGenerators)
import Hickory.Input (makeTimePoller)
import Platforms.GLFW (getGLFWWindowSizeRef, makeGLFWInputPoller)
import qualified Graphics.UI.GLFW as GLFW

glfwCoreEventGenerators :: GLFW.Window -> IO (a -> IO (), CoreEventGenerators a)
glfwCoreEventGenerators win = do
  wSizeRef    <- getGLFWWindowSizeRef win
  inputPoller <- makeGLFWInputPoller win
  timePoller  <- makeTimePoller
  coreEventGenerators inputPoller timePoller wSizeRef
