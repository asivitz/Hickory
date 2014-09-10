{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Engine.Run where

import Engine.Model
import Data.Time
import Math.Matrix
import Types.Types
import Camera.Camera

import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB.GeometryShader4
import qualified Graphics.UI.GLFW as GLFW
import Data.Bits
import Graphics.Drawing
import Graphics.GLFWUtils
import Engine.Input
import Control.Lens
import qualified Systems.GLFWPlatform as GLFWPlatform

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

calcMatrixFromModel :: Size Int -> Model cs -> Mat44
calcMatrixFromModel scrSize model = let ar = aspectRatio scrSize in
    cameraMatrix (_camera model) ar


makeStepFunc :: IO a -> (a -> b -> Double -> c -> c) -> (b -> Double -> c -> IO c)
makeStepFunc inputFunc stepFunc = \ri delta model -> do
    input <- inputFunc
    return $ stepFunc input ri delta model


iter :: RenderInfo -> (Mat44 -> Model cs -> IO ()) -> (RenderInfo -> Double -> Model cs -> IO (Model cs)) -> Model cs -> UTCTime -> IO ()
iter !ri@(RenderInfo _ ss) !render !step !model !prev_time = do
        current_time <- getCurrentTime
        let delta = min 0.1 $ realToFrac (diffUTCTime current_time prev_time)

        model' <- step ri delta model
        let matrix' = (calcMatrixFromModel ss model')
        render matrix' model'

        iter (RenderInfo matrix' ss) render step model' current_time

run :: Size Int -> (Mat44 -> Model cs -> IO ()) -> (RenderInfo -> Double -> Model cs -> IO (Model cs)) -> Model cs -> IO ()
run scrSize render step model = do
        ct <- getCurrentTime

        iter (RenderInfo (calcMatrixFromModel scrSize model) scrSize) render step model ct

{-initAndRun :: World r -> SysMonad r IO [System r] -> IO ()-}
{-initAndRun w initF = do-}
        {-(systems, w') <- runStateT initF w-}
        {-run w' systems-}

{-newWorldWithResourcesPath :: Context cs rsc -> String -> World (Context cs rsc)-}
{-newWorldWithResourcesPath context path =-}
        {-registerResourceToWorld sysCon (emptyWorld context) resourcesPath (return path)-}

stepModel :: (RenderInfo -> InputEv -> Model cs -> Model cs) ->
    (Double -> cs -> cs) -> 
    Input -> RenderInfo -> Double -> Model cs -> Model cs
stepModel procInputF stepCompF Input { inputEvents } ri delta model = let model' = foldr (procInputF ri) model inputEvents 
                                                                          model'' = over components (\cs -> stepCompF delta cs) model'
                                                                          in model''

glfwRender :: GLFW.Window -> (Model cs -> IO ()) -> Mat44 -> Model cs -> IO ()
glfwRender win renderFunc matrix model = do
        renderFunc model

        glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)

        renderCommands matrix uiLabel

        resetRenderer
        GLFW.swapBuffers win

glfwMain :: Camera -> cs -> IO r -> (RenderInfo -> InputEv -> Model cs -> Model cs) ->
    (Double -> cs -> cs) ->
    (r -> Model cs -> IO ()) ->
    IO ()
glfwMain camera comps recLoadF procInputF stepCompsF renderF = do 
          withWindow 640 480 "MVC!" $ \win -> do
              initRenderer
              glClearColor 0.3 0.5 0 1
              glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA
              glActiveTexture gl_TEXTURE0
                
              glEnable gl_PROGRAM_POINT_SIZE -- for OSX

              (width, height) <- GLFW.getFramebufferSize win

              resources <- recLoadF

              grabInputFunc <- GLFWPlatform.makeGrabInput win

              run (Size width height) 
                  (glfwRender win (renderF resources)) 
                  (makeStepFunc grabInputFunc (stepModel procInputF stepCompsF))
                  (newModel camera comps)
