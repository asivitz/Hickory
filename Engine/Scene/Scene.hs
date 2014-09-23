{-# LANGUAGE NamedFieldPuns #-}

module Engine.Scene.Scene where

import Data.IORef
import Engine.Scene.Input
import Types.Types
import Graphics.Drawing
import Math.Matrix

data RenderInfo = RenderInfo Mat44 (Size Int) Label

data Scene mdl ie re = Scene {
                       _name :: String,
                       _model :: mdl,
                       _renderInfo :: RenderInfo,
                       _loadResources :: IO re,
                       _stepModel :: RenderInfo -> Input ie -> Double -> mdl -> (mdl, [ie]),
                       _render :: re -> RenderInfo -> mdl -> IO (),
                       _inputStream :: IORef (Input ie),
                       -- Filled after resources are loaded
                       _loadedRender :: Maybe (RenderInfo -> mdl -> IO ())
                       }

instance Show (Scene mdl c d) where
        show Scene { _name } = "Scene: " ++ _name

class SceneModel mdl where
        calcCameraMatrix :: Size Int -> mdl -> Mat44

-- A SceneOperator provides an interface to a Scene, such that the
-- operations are connected by an IORef. The SceneOperator provides no
-- knowledge of the Scene's types, so it can be put in a list and
-- mapped/folded.
data SceneOperator ie = SceneOperator {
                      _initRenderer :: Size Int -> IO (),
                      _step :: Double -> IO [ie],
                      _addEvent :: ie -> IO (),
                      _renderOp :: IO ()
                      }

makeSceneOperator :: (SceneModel mdl, Show ie) => Scene mdl ie re -> IO (SceneOperator ie)
makeSceneOperator scene = do
        ref <- newIORef scene
        return $ SceneOperator { 
            _initRenderer = (\scrSize -> do
                    scn@Scene {_loadResources, 
                              _render, 
                              _model, 
                              _renderInfo = (RenderInfo _ _ label) } 
                              <- readIORef ref
                    res <- _loadResources
                    writeIORef ref scn { _loadedRender = Just (_render res),
                                       _renderInfo = RenderInfo (calcCameraMatrix scrSize _model) scrSize label }),
            _step = (\delta -> do
                    scn <- readIORef ref
                    (scn', evs) <- stepScene scn delta
                    writeIORef ref scn'
                    return evs
                    ),
            _addEvent = (\ev -> do
                        scn <- readIORef ref
                        addSceneInput scn ev),
            _renderOp = do
                scn <- readIORef ref
                renderCommandsForScene scn
                }

addSceneInput :: Scene mdl ie re -> ie -> IO ()
addSceneInput Scene { _inputStream } ev = 
        atomicModifyIORef _inputStream (\(Input evs) -> (Input (ev:evs), ()))

renderCommandsForScene Scene { _renderInfo = (RenderInfo mat _ label) } = renderCommands mat label

grabSceneInput :: Show ie => Scene mdl ie re -> IO (Input ie)
grabSceneInput Scene { _inputStream } = do
        input <- atomicModifyIORef _inputStream (\i -> (Input { inputEvents = [] }, i))
        {-print input-}
        return input

stepScene :: (Show ie, SceneModel mdl) => Scene mdl ie re -> Double -> IO (Scene mdl ie re, [ie])
stepScene scene@Scene { _loadedRender = Just renderFunc, 
                      _model, 
                      _renderInfo = (ri@(RenderInfo _ ss label)), 
                      _stepModel } 
                      delta = do
        input <- grabSceneInput scene
        let (model', outEvents) = _stepModel ri input delta _model 
            matrix' = (calcCameraMatrix ss model')
            scene' = scene { _model = model', _renderInfo = (RenderInfo matrix' ss label) }
        renderFunc ri model'
        return (scene', outEvents)
stepScene scene _ = print "Couldn't step scene." >> return (scene, [])

makeStepModel :: (RenderInfo -> ie -> model -> (model, [ie])) ->
    (Double -> model -> model) -> 
    RenderInfo -> Input ie -> Double -> model -> (model, [ie])
makeStepModel procInputF stepCompF ri Input { inputEvents } delta model = 
        let accum (m, oes) ie = let (m', oes') = procInputF ri ie m in (m', oes' ++ oes)
            (model', outputEvents) = foldl accum (model,[]) inputEvents 
            model'' = stepCompF delta model'
            in (model'', outputEvents)
