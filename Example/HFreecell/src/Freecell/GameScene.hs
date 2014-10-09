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
import Data.Maybe
import Utils.Utils
import Control.Monad.State
import Utils.Projection

spawnCard pos card = do
        let scale = 1
        e <- spawnEntity
        addComp e drawStates $ DrawState pos
        addComp e selectables $ Selectable (Size (0.726 * scale) scale)
        addComp e cardComps card
        {-addComp e mouseDrags $ MouseDrag (v3 0 0 0)-}
        {-addComp components e newtonianMovers $ NewtonianMover (v3 40 16 0) (v3 0 0 0)-}
        return []

pickSelectable pos (e, (Selectable size), card, (DrawState p)) =
        posInRect (v3tov2 pos) (Rect (v3tov2 p) size)

-- swap $ runModel (spawnThing p') model

processInput :: RenderInfo -> InputEvent -> Model ComponentStore GameModel -> (Model ComponentStore GameModel, [InputEvent])
processInput renderinfo (RawEvent (InputTouchDown pos pid)) model = forModel model $ do
        let zipped = zipComps3 model selectables cardComps drawStates
            depth :: (e, Selectable, Card, DrawState) -> Maybe Int
            depth (e, _, card, _) = cardDepth (getBoard model) card
            selected = listToMaybe . sortOnMaybe depth . filter (pickSelectable unproj) $ zipped

        whenMaybe selected $ (\(e, _, _, (DrawState p)) ->
            addComp e mouseDrags $ MouseDrag (p - unproj))
        return []
    where unproj = unproject pos (-5) renderinfo

processInput renderinfo (RawEvent (InputTouchUp time pos pid)) model = forModel model $ do
    let board = getBoard model
        unproj = unproject pos (-5) renderinfo
        comps = zipComps2 model mouseDrags cardComps
        targetLocation = dropLocationForPos board (v3tov2 unproj)

    case listToMaybe comps of
        Nothing -> return []
        Just (e, _, card) -> do
            removeComp e mouseDrags

            case moveCard board card targetLocation of
                x | solvedBoard x -> return [WonGame]
                x | null $ allPermissable x -> return [LostGame]
                x | otherwise -> do 
                                    model' <- get
                                    put $ setBoard model' x
                                    return []

processInput renderinfo (RawEvent (InputTouchLoc pos pid)) model = 
        noEvents $ over components (\cs -> upComps2 cs drawStates mouseDrags (DrawState.snapToMouse p')) model
    where p' = unproject pos (-5) renderinfo

processInput renderinfo NewGame model =
        forModel model $ do
            mapM_ (\c -> spawnCard (v3 0 0 (-5)) c) allCards
            return []

processInput _ _ model = noEvents model

stepComponents :: Double -> Model ComponentStore GameModel -> (Model ComponentStore GameModel, [InputEvent])
stepComponents delta model = noEvents $ 
        model { _components = upComps2Ent (_components model) drawStates cardComps (upCardDS delta (getBoard model) model) }

makeScene = do
        board <- makeGame
        is <- newIORef (Input [])
        let cam = \size -> Camera (Ortho 10 1 100) (Route pZero Nothing)
            scene = Scene {
                          _name = "Game",
                          _model = (newModel cam emptyComponentStore (GameModel board)),
                          _renderInfo = RenderInfo mat44Identity nullSize worldLabel,
                          _loadResources = loadResources "Example/HFreecell/resources",
                          _stepModel = makeStepModel processInput stepComponents,
                          _render = render,
                          _inputStream = is,
                          _loadedRender = Nothing }
        return scene

upCardDS delta board model e (DrawState p) card =
      let delta' = realToFrac delta 
          pilePos = posForCard board card 
          md = modelCompForEnt model e mouseDrags
          in case md of
                  -- Only move toward pile if we're not dragging
                  Nothing -> DrawState (movePos p pilePos 10 delta')
                  _ -> DrawState p
