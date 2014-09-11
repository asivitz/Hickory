{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Engine.Run where

import Engine.Model
import Data.Time
import Math.Matrix
import Types.Types
import Camera.Camera
import Data.IORef
import Data.List

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
import Utils.Utils

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

stepScene :: Show ie => Scene cs gm ie re -> Double -> IO (Scene cs gm ie re, [ie])
stepScene scene@Scene { _loadedRender = Just renderFunc, 
                      _model, 
                      _renderInfo = (ri@(RenderInfo _ ss label)), 
                      _stepModel } 
                      delta = do
        input <- grabSceneInput scene
        let (model', outEvents) = _stepModel ri input delta _model 
            matrix' = (calcMatrixFromModel ss model')
            scene' = scene { _model = model', _renderInfo = (RenderInfo matrix' ss label) }
        renderFunc model'
        return (scene', outEvents)
stepScene scene _ = print "Couldn't step scene." >> return (scene, [])

mapWithOthers :: (a -> [a] -> b) -> [a] -> [b]
mapWithOthers fun lst = sub fun [] lst
    where sub f prevxs (x:xs) = f x (prevxs ++ xs) : sub f (x:prevxs) xs
          sub f _ [] = []

iter :: Show ie => IO () -> [Scene cs gm ie re] -> ([Scene cs gm ie re] -> IO ()) -> UTCTime -> IO ()
iter !stepInp !scenes !renderFunc !prev_time = do
        current_time <- getCurrentTime
        let delta = min 0.1 $ realToFrac (diffUTCTime current_time prev_time)

        stepInp

        scenes' <- sequence . mapWithOthers (\scene others -> do
             (scene', outEvents) <- stepScene scene delta
             mapM_ (\otherScene -> mapM_ (\ie -> addSceneInput otherScene ie) outEvents) others
             return scene')
             $ scenes

        renderFunc scenes'

        iter stepInp scenes' renderFunc current_time

run :: Show ie => IO () -> [Scene cs gm ie re] -> ([Scene cs gm ie re] -> IO ()) -> IO ()
run stepInp scenes renderFunc = do
        ct <- getCurrentTime

        iter stepInp scenes renderFunc ct

makeStepModel :: (RenderInfo -> ie -> Model cs gm -> (Model cs gm, [ie])) ->
    (Double -> cs -> cs) -> 
    RenderInfo -> Input ie -> Double -> Model cs gm -> (Model cs gm, [ie])
makeStepModel procInputF stepCompF ri Input { inputEvents } delta model = 
        let accum (m, oes) ie = let (m', oes') = procInputF ri ie m in (m', oes' ++ oes)
            (model', outputEvents) = foldl accum (model,[]) inputEvents 
            model'' = over components (\cs -> stepCompF delta cs) model'
            in (model'', outputEvents)

renderCommandsForScene Scene { _renderInfo = (RenderInfo mat _ label) } = renderCommands mat label

glfwRender :: GLFW.Window -> [Scene cs gm ie re] -> IO ()
glfwRender win scenes = do
        glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)

        mapM_ (\scene -> renderCommandsForScene scene) scenes

        resetRenderer
        GLFW.swapBuffers win

grabSceneInput :: Show ie => Scene cs gm ie re -> IO (Input ie)
grabSceneInput Scene { _inputStream } = do
        input <- atomicModifyIORef _inputStream (\i -> (Input { inputEvents = [] }, i))
        return input

addSceneInput :: Scene cs gm ie re -> ie -> IO ()
addSceneInput Scene { _inputStream } ev = 
        atomicModifyIORef _inputStream (\(Input evs) -> (Input (ev:evs), ()))

data Scene cs gm ie re = Scene {
                       _model :: Model cs gm,
                       _renderInfo :: RenderInfo,
                       _loadResources :: IO re,
                       _stepModel :: RenderInfo -> Input ie -> Double -> Model cs gm -> (Model cs gm, [ie]),
                       _render :: re -> Model cs gm -> IO (),
                       _inputStream :: IORef (Input ie),
                       -- Filled after resources are loaded
                       _loadedRender :: Maybe (Model cs gm -> IO ())
                       }

glfwMain :: Show ie => [Scene cs gm ie re] -> (RawInput -> ie) -> IO ()
glfwMain scenes pkgRawInput = do 
          withWindow 640 480 "MVC!" $ \win -> do
              initRenderer
              glClearColor 0.3 0.5 0 1
              glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA
              glActiveTexture gl_TEXTURE0
                
              glEnable gl_PROGRAM_POINT_SIZE -- for OSX

              (width, height) <- GLFW.getFramebufferSize win

              let scrSize = (Size width height)

              scenes' <- mapM (\scene@Scene { _loadResources, _render, _model, _renderInfo = (RenderInfo _ _ label) } -> do
                                    res <- _loadResources
                                    return $ scene { _loadedRender = Just (_render res),
                                                     _renderInfo = RenderInfo (calcMatrixFromModel scrSize _model) scrSize label
                                                   })
                              scenes

              whenMaybe (listToMaybe scenes') $ \firstScene -> do
                  stepInp <- GLFWPlatform.setupInput win (\raw -> addSceneInput firstScene (pkgRawInput raw))
                  run stepInp
                      scenes'
                      (glfwRender win)
