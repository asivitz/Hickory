{-# LANGUAGE NamedFieldPuns #-}

module Freecell.GameScene (makeScene, InputEvent(..)) where

import FreeCell
import Freecell.Events
import Freecell.Utils
import Engine.Component.Component
import Engine.Component.CompUtils
import Engine.Component.Model
import Engine.Scene.Input
import Engine.Scene.Scene
import Math.Matrix
import Math.VectorMatrix
import Types.Types
import Data.IORef
import Graphics.Drawing
import Control.Lens
import Camera.Camera
import qualified Systems.DrawState as DrawState
import Math.Vector
import Freecell.Render
import Freecell.Component

spawnCard pos card = do
        let scale = 1
        e <- spawnEntity
        addComp e drawStates $ DrawState pos
        addComp e selectables $ Selectable (Size (0.726 * scale) scale)
        addComp e cardComps card
        {-addComp e mouseDrags $ MouseDrag (v3 0 0 0)-}
        {-addComp components e newtonianMovers $ NewtonianMover (v3 40 16 0) (v3 0 0 0)-}
        return []

processInput :: RenderInfo -> InputEvent -> Model ComponentStore GameModel -> (Model ComponentStore GameModel, [InputEvent])
{-processInput (RenderInfo mat ss _) (RawEvent (InputTouchDown pos pid)) model = swap $ runModel (spawnThing p') model-}
    {-where p' = lerpUnproject pos (-5) mat (viewportFromSize ss)-}

processInput (RenderInfo mat ss _) (RawEvent (InputTouchLoc pos pid)) model = 
        (over components (\cs -> upComps2 cs drawStates mouseDrags (DrawState.snapToMouse p')) model, [])
    where p' = lerpUnproject pos (-5) mat (viewportFromSize ss)

processInput (RenderInfo mat ss _) NewGame model@Model { _game = GameModel { _gameBoard } } =
        forModel model $ do
            mapM_ (\c -> spawnCard (v3 0 0 (-5)) c) (allCards _gameBoard)
            return []

processInput _ _ model = (model, [])

stepComponents :: Double -> Model ComponentStore GameModel -> Model ComponentStore GameModel
stepComponents delta model@Model { _game = GameModel { _gameBoard }, _components } =
        model { _components = upComps2Ent _components drawStates cardComps (upCardDS delta _gameBoard model) }

makeScene = do
        board <- makeGame
        is <- newIORef (Input [])
        let cam = Camera (Ortho 10 1 100) (Route pZero Nothing)
            scene = Scene {
                          _name = "Game",
                          _model = (newModel cam emptyComponentStore (GameModel board)),
                          _renderInfo = RenderInfo mat44Identity nullSize uiLabel,
                          _loadResources = loadResources "Example/HFreecell/resources",
                          _stepModel = makeStepModel processInput stepComponents,
                          _render = render,
                          _inputStream = is,
                          _loadedRender = Nothing }
        return scene

--

upCardDS delta board model e (DrawState p) card =
      let delta' = realToFrac delta 
          pilePos = posForCard board card 
          md = modelCompForEnt model e mouseDrags
          in case md of
                  -- Only move toward pile if we're not dragging
                  Nothing -> DrawState (movePos p pilePos 10 delta')
                  _ -> DrawState p
