module Graphics.GLFWUtils (buildWindow) where

import qualified Graphics.UI.GLFW          as GLFW

buildWindow :: Int -> Int -> String -> IO (Maybe GLFW.Window)
buildWindow width height title = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
    GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 2)
    if r then do
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              GLFW.setErrorCallback $ Just simpleErrorCallback
              return $ Just win
          Nothing -> do
              print "ERROR: Couldn't create window"
              return Nothing
        else return Nothing
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]
