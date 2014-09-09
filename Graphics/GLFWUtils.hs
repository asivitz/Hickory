module Graphics.GLFWUtils (buildWindow, withWindow) where

import qualified Graphics.UI.GLFW          as GLFW
import Control.Monad

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

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
    GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 2)
    when r $ do
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              GLFW.setErrorCallback $ Just simpleErrorCallback
              f win
              GLFW.destroyWindow win
              GLFW.terminate
          Nothing -> do
              print "ERROR: Couldn't create window"
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]
