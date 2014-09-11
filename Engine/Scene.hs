{-# LANGUAGE NamedFieldPuns #-}

module Engine.Scene where

import Data.IORef
import Engine.Model
import Engine.Input
import Types.Types
import Graphics.Drawing
import Math.Matrix
import Control.Lens
import Camera.Camera

data Scene cs gm ie re = Scene {
                       _name :: String,
                       _model :: Model cs gm,
                       _renderInfo :: RenderInfo,
                       _loadResources :: IO re,
                       _stepModel :: RenderInfo -> Input ie -> Double -> Model cs gm -> (Model cs gm, [ie]),
                       _render :: re -> Model cs gm -> IO (),
                       _inputStream :: IORef (Input ie),
                       -- Filled after resources are loaded
                       _loadedRender :: Maybe (Model cs gm -> IO ())
                       }

instance Show (Scene a b c d) where
        show Scene { _name } = "Scene: " ++ _name

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
                                       _renderInfo = RenderInfo (calcMatrixFromModel scrSize _model) scrSize label }),
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

addSceneInput :: Scene cs gm ie re -> ie -> IO ()
addSceneInput Scene { _inputStream } ev = 
        atomicModifyIORef _inputStream (\(Input evs) -> (Input (ev:evs), ()))

renderCommandsForScene Scene { _renderInfo = (RenderInfo mat _ label) } = renderCommands mat label

grabSceneInput :: Show ie => Scene cs gm ie re -> IO (Input ie)
grabSceneInput Scene { _inputStream } = do
        input <- atomicModifyIORef _inputStream (\i -> (Input { inputEvents = [] }, i))
        {-print input-}
        return input

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

makeStepModel :: (RenderInfo -> ie -> Model cs gm -> (Model cs gm, [ie])) ->
    (Double -> cs -> cs) -> 
    RenderInfo -> Input ie -> Double -> Model cs gm -> (Model cs gm, [ie])
makeStepModel procInputF stepCompF ri Input { inputEvents } delta model = 
        let accum (m, oes) ie = let (m', oes') = procInputF ri ie m in (m', oes' ++ oes)
            (model', outputEvents) = foldl accum (model,[]) inputEvents 
            model'' = over components (\cs -> stepCompF delta cs) model'
            in (model'', outputEvents)

calcMatrixFromModel :: Size Int -> Model cs gm -> Mat44
calcMatrixFromModel scrSize model = let ar = aspectRatio scrSize in
    cameraMatrix (_camera model) ar


