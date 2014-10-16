{-# LANGUAGE ForeignFunctionInterface #-}

module IOS.Run where

import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB.GeometryShader4
import Data.Bits
import Graphics.Drawing
import Engine.Scene.Scene
import Engine.Scene.Run
import Types.Types

import Foreign.Ptr
import Control.Concurrent
import Control.Monad
import Data.IORef
import Foreign
import Foreign.C

iosInit :: [SceneOperator ie] -> IO ()
iosInit operators = do
        initRenderer
        glClearColor 0.125 0.125 0.125 1
        glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA
        glActiveTexture gl_TEXTURE0
        
        glEnable gl_PROGRAM_POINT_SIZE -- for OSX

        {-(width, height) <- GLFW.getFramebufferSize win-}

        let scrSize = (Size 100 100)

        sequence_ $ mapAll (map _initRenderer operators) scrSize

iosRender :: [SceneOperator ie] -> IO ()
iosRender operators = do
        glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)

        mapM_ _renderOp operators

        resetRenderer

mapAll :: [a -> b] -> a -> [b]
mapAll fs a = map (\f -> f a) fs
