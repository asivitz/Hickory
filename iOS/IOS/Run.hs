{-# LANGUAGE ForeignFunctionInterface #-}

module IOS.Run where

import Graphics.Rendering.OpenGL.Raw.Core31
import Data.Bits
import Graphics.Drawing
import Engine.Scene.Scene
import Engine.Scene.Run
import Types.Types
import Data.Time
import Data.IORef

import Foreign.C

makeIOSStepFunc :: Show ie => [SceneOperator ie] -> IO (IO ())
makeIOSStepFunc operators = do
        start_time <- getCurrentTime
        timeref <- newIORef start_time

        return $ do
            prev_time <- readIORef timeref
            current_time <- getCurrentTime
            let delta = min 0.1 $ realToFrac (diffUTCTime current_time prev_time)
            writeIORef timeref current_time

            runOneFrame (return ()) operators iosRender delta

iosInitFunc :: [SceneOperator ie] -> CInt -> CInt -> IO ()
iosInitFunc ops w h = iosInit (Size (fromIntegral w) (fromIntegral h)) ops

iosInit :: Size Int -> [SceneOperator ie] -> IO ()
iosInit scrSize operators = do
        initRenderer
        glClearColor 0.125 0.125 0.125 1
        glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA
        glActiveTexture gl_TEXTURE0
        
        {-glEnable gl_PROGRAM_POINT_SIZE -- for OSX-}

        mapM_ (\op -> (_initRenderer op) scrSize) operators

iosRender :: [SceneOperator ie] -> IO ()
iosRender operators = do
        glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)

        mapM_ _renderOp operators

        resetRenderer

mapAll :: [a -> b] -> a -> [b]
mapAll fs a = map (\f -> f a) fs
