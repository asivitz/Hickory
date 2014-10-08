{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Engine.Scene.Run where

import Data.Time
import Types.Types

import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB.GeometryShader4
import qualified Graphics.UI.GLFW as GLFW
import Data.Bits
import Graphics.Drawing
import Graphics.GLFWUtils
import qualified Systems.GLFWPlatform as GLFWPlatform
import Data.Maybe
import Utils.Utils
import Engine.Scene.Input
import Engine.Scene.Scene

{-
governFPS :: UTCTime -> IO ()
governFPS initialTime = do
   -- I have no idea when this stuff actually runs bc of laziness
   after_time <- getCurrentTime
   let elapsed = realToFrac (diffUTCTime after_time initialTime)
       millisecondsEarly = 16.66 - elapsed * 1000 :: Double

   when (millisecondsEarly > 0) $
      threadDelay $ floor (millisecondsEarly * 1000)
      -}

{-simulate :: World c -> [System c] -> Double -> IO (World c)-}
{-simulate world systems delta = execStateT (mapM_ (`runSys` delta) systems) world-}

mapWithOthers :: (a -> [a] -> b) -> [a] -> [b]
mapWithOthers fun lst = sub fun [] lst
    where sub f prevxs (x:xs) = f x (prevxs ++ xs) : sub f (x:prevxs) xs
          sub f _ [] = []

iter :: Show ie => IO () -> [SceneOperator ie] -> ([SceneOperator ie] -> IO ()) -> UTCTime -> IO ()
iter !stepInp !operators !renderFunc !prev_time = do
        current_time <- getCurrentTime
        let delta = min 0.1 $ realToFrac (diffUTCTime current_time prev_time)

        stepInp

        sequence_ . mapWithOthers (\oper others -> do
             outEvents <- (_step oper) delta
             mapM_ (\otherOp -> mapM_ (\ie -> (_addEvent otherOp) ie) outEvents) others
             )
             $ operators

        renderFunc operators

        iter stepInp operators renderFunc current_time

run :: Show ie => IO () -> [SceneOperator ie] -> ([SceneOperator ie] -> IO ()) -> IO ()
run stepInp operators renderFunc = do
        ct <- getCurrentTime

        iter stepInp operators renderFunc ct

glfwRender :: GLFW.Window -> [SceneOperator ie] -> IO ()
glfwRender win operators = do
        glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)

        mapM_ _renderOp operators

        resetRenderer
        GLFW.swapBuffers win

mapAll :: [a -> b] -> a -> [b]
mapAll fs a = map (\f -> f a) fs

glfwMain :: Show ie => Size Int -> [SceneOperator ie] -> SceneOperator ie -> (RawInput -> ie) -> IO ()
glfwMain (Size w h) operators keyOperator pkgRawInput = do 
          withWindow w h "MVC!" $ \win -> do
              initRenderer
              glClearColor 0.125 0.125 0.125 1
              glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA
              glActiveTexture gl_TEXTURE0
                
              glEnable gl_PROGRAM_POINT_SIZE -- for OSX

              (width, height) <- GLFW.getFramebufferSize win

              let scrSize = (Size width height)

              sequence_ $ mapAll (map _initRenderer operators) scrSize

              stepInp <- GLFWPlatform.setupInput win (\raw -> (_addEvent keyOperator (pkgRawInput raw)))
              run stepInp
                  operators
                  (glfwRender win)
