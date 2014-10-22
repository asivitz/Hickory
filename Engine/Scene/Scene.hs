{-# LANGUAGE NamedFieldPuns #-}

module Engine.Scene.Scene where

import Data.IORef
import Types.Types
import Graphics.Drawing
import Math.Matrix

-- Each frame, the RenderInfo struct provides the matrix, screen size, 
-- and layer used to render the previous frame
data RenderInfo = RenderInfo Mat44 (Size Int) Label deriving Show

-- mdl - The model used to represent the data for this Scene
-- ie - The InputEvent data type shared by all scenes
-- re - The resources loaded by this scene
data Scene mdl ie re = Scene {
                       _model :: mdl,
                       _renderInfo :: RenderInfo,
                       _stepModel :: ModelStep mdl ie,
                       _render :: Render mdl,
                       _inputStream :: IORef ([ie])
                       }

type ModelStep mdl ie = RenderInfo -> [ie] -> Double -> mdl -> (mdl, [ie])
type Render mdl = RenderInfo -> mdl -> IO ()

instance Show (Scene mdl c d) where
        show scn = "Scene"

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

-- Wraps a Scene up into a SceneOperator
makeSceneOperator :: (SceneModel mdl, Show ie) => mdl -> IO re -> ModelStep mdl ie -> (re -> Render mdl) -> Label -> IO (SceneOperator ie)
makeSceneOperator model resourceLoader modelStep render label = do
        ref <- newIORef Nothing
        is <- newIORef []
        return $ SceneOperator { 
            _initRenderer = (\scrSize -> do
                    res <- resourceLoader
                    writeIORef ref $ Just (Scene model 
                                               (RenderInfo (calcCameraMatrix scrSize model) scrSize label)
                                               modelStep
                                               (render res)
                                               is)
                                               ),
            _step = (\delta -> do
                    mscn <- readIORef ref
                    case mscn of
                        Just scn -> do
                            (scn', evs) <- stepScene scn delta
                            writeIORef ref $ Just scn'
                            return evs
                        Nothing -> return []
                    ),
            _addEvent = (\ev -> do
                    mscn <- readIORef ref
                    case mscn of
                        Just scn -> do
                            addSceneInput scn ev
                        Nothing -> return ()
                        ),
            _renderOp = do
                mscn <- readIORef ref
                case mscn of
                    Just scn -> do
                        renderCommandsForScene scn
                    Nothing -> return ()
                }

addSceneInput :: Scene mdl ie re -> ie -> IO ()
addSceneInput Scene { _inputStream } ev = 
        atomicModifyIORef _inputStream (\evs -> ((evs ++ [ev]), ()))

renderCommandsForScene Scene { _renderInfo = (RenderInfo mat _ label) } = renderCommands mat label

grabSceneInput :: Show ie => Scene mdl ie re -> IO ([ie])
grabSceneInput Scene { _inputStream } = do
        input <- atomicModifyIORef _inputStream (\i -> ([], i))
        {-print input-}
        return input

stepScene :: (Show ie, SceneModel mdl) => Scene mdl ie re -> Double -> IO (Scene mdl ie re, [ie])
stepScene scene@Scene { _render = renderFunc, 
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

-- Utility method for building a step function out of an event handler and
-- a generic step function
makeStepModel :: (RenderInfo -> ie -> model -> (model, [ie])) ->
    (Double -> model -> (model, [ie])) -> 
    RenderInfo -> [ie] -> Double -> model -> (model, [ie])
makeStepModel procInputF stepCompF ri inputEvents delta model = 
        let accum (m, oes) ie = let (m', oes') = procInputF ri ie m 
                in (m', oes ++ oes')
            (model', outputEvents) = foldl accum (model,[]) inputEvents 
            (model'', stepOutputEvents) = stepCompF delta model'
            in (model'', outputEvents ++ stepOutputEvents)

-- Utililty method for return a model when there are no new events to
-- distribute
noEvents :: model -> (model, [ie])
noEvents model = (model, [])
