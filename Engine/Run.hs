{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Engine.Run where

import Engine.Model
import Data.Time
import Math.Matrix
import Types.Types
import Camera.Camera
import Data.IORef

import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB.GeometryShader4
import qualified Graphics.UI.GLFW as GLFW
import Data.Bits
import Graphics.Drawing
import Graphics.GLFWUtils
import Engine.Input
import Control.Lens
import qualified Systems.GLFWPlatform as GLFWPlatform
import Data.Maybe

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

calcMatrixFromModel :: Size Int -> Model cs gm -> Mat44
calcMatrixFromModel scrSize model = let ar = aspectRatio scrSize in
    cameraMatrix (_camera model) ar


iter :: RenderInfo -> (Mat44 -> Model cs gm -> IO ()) -> (RenderInfo -> Double -> Model cs gm -> IO (Model cs gm)) -> Model cs gm -> UTCTime -> IO ()
iter !ri@(RenderInfo _ ss) !render !step !model !prev_time = do
        current_time <- getCurrentTime
        let delta = min 0.1 $ realToFrac (diffUTCTime current_time prev_time)

        model' <- step ri delta model
        let matrix' = (calcMatrixFromModel ss model')
        render matrix' model'

        iter (RenderInfo matrix' ss) render step model' current_time

run :: Size Int -> (Mat44 -> Model cs gm -> IO ()) -> (RenderInfo -> Double -> Model cs gm -> IO (Model cs gm)) -> Model cs gm-> IO ()
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

makeStepModel :: (RenderInfo -> ie -> Model cs gm -> Model cs gm) ->
    (Double -> cs -> cs) -> 
    RenderInfo -> Input ie -> Double -> Model cs gm -> Model cs gm
makeStepModel procInputF stepCompF ri Input { inputEvents } delta model = let model' = foldr (procInputF ri) model inputEvents 
                                                                              model'' = over components (\cs -> stepCompF delta cs) model'
                                                                              in model''

glfwRender :: GLFW.Window -> (Model cs gm -> IO ()) -> Mat44 -> Model cs gm -> IO ()
glfwRender win renderFunc matrix model = do
        renderFunc model

        glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)

        renderCommands matrix uiLabel

        resetRenderer
        GLFW.swapBuffers win

grabSceneInput :: Scene cs gm ie re -> IO (Input ie)
grabSceneInput Scene { _inputStream } = do
        input <- atomicModifyIORef _inputStream (\i -> (Input { inputEvents = [] }, i))
        return input

addSceneInput :: Scene cs gm ie re -> ie -> IO ()
addSceneInput Scene { _inputStream } ev = 
        atomicModifyIORef _inputStream (\(Input evs) -> (Input (ev:evs), ()))

data Scene cs gm ie re = Scene {
                    _loadResources :: IO re,
                    _stepModel :: RenderInfo -> Input ie -> Double -> Model cs gm -> Model cs gm,
                    _render :: re -> Model cs gm -> IO (),
                    _inputStream :: IORef (Input ie),
                    -- Filled after resources are loaded
                    _loadedRender :: Maybe (Model cs gm -> IO ())
                    }

makeStepFunc :: IO () -> Scene cs gm ie re -> (RenderInfo -> Double -> Model cs gm -> IO (Model cs gm))
makeStepFunc stepInp scene = \ri delta model -> do
    stepInp
    input <- grabSceneInput scene
    return $ case scene of
        Scene { _stepModel } -> _stepModel ri input delta model 

glfwMain :: Camera -> cs -> gm -> Scene cs gm ie re -> (RawInput -> ie) -> IO ()
glfwMain cam comps gm scene pkgRawInput = do 
          withWindow 640 480 "MVC!" $ \win -> do
              initRenderer
              glClearColor 0.3 0.5 0 1
              glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA
              glActiveTexture gl_TEXTURE0
                
              glEnable gl_PROGRAM_POINT_SIZE -- for OSX

              (width, height) <- GLFW.getFramebufferSize win

              scene' <- case scene of
                            Scene { _loadResources, _render } -> do
                                res <- _loadResources
                                return $ scene { _loadedRender = Just (_render res) }

              stepInp <- GLFWPlatform.setupInput win (\raw -> addSceneInput scene (pkgRawInput raw))

              run (Size width height) 
                  (glfwRender win (fromJust (_loadedRender scene')))
                  (makeStepFunc stepInp scene')
                  (newModel cam comps gm)
