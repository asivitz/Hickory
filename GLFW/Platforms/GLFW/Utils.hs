{-# LANGUAGE NamedFieldPuns, BlockArguments #-}

module Platforms.GLFW.Utils (buildWindow, withWindow, createGBufferRef, onFramebufferSize) where

import qualified Graphics.UI.GLFW          as GLFW
import Control.Monad
import qualified Graphics.GL.Compatibility41 as GL
import Data.IORef (newIORef, writeIORef, readIORef, IORef)
import Hickory.Types (Size(..))
import Hickory.Graphics (GBuffer(..), createGBuffer, getTexID)
import Hickory.Graphics.GLSupport (withArrayLen)

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
              print ("ERROR: Couldn't create window" :: String)
              return Nothing
        else return Nothing
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]

onFramebufferSize :: GLFW.Window -> (Size Int -> IO ()) -> IO ()
onFramebufferSize win f = GLFW.setFramebufferSizeCallback win . Just $ \_ w h -> do
  f (Size w h)

  GL.glViewport 0 0 (fromIntegral w) (fromIntegral h)

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

              onFramebufferSize win (const $ pure ())

              f win

              GLFW.destroyWindow win
              GLFW.terminate

          Nothing -> do
              print ("ERROR: Couldn't create window" :: String)
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]

-- Dynamically recreates GBuffer as the framebuffer size changes
createGBufferRef :: GLFW.Window -> IO (IORef GBuffer)
createGBufferRef win = do
  (fbw, fbh) <- GLFW.getFramebufferSize win
  ref <- createGBuffer (Size fbw fbh) >>= newIORef
  GLFW.setFramebufferSizeCallback win . Just $ \_win w h -> do
    GBuffer { frameBuffer, position, normal, albedo } <- readIORef ref
    withArrayLen [frameBuffer] GL.glDeleteFramebuffers
    withArrayLen (map getTexID [position, normal, albedo]) GL.glDeleteTextures
    createGBuffer (Size w h) >>= writeIORef ref

    GL.glViewport 0 0 (fromIntegral w) (fromIntegral h) -- Just as the typical callback does
  pure ref
