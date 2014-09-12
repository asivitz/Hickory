{-# LANGUAGE NamedFieldPuns #-}

module Freecell.Systems.Game (make) where

import Control.Monad
import Control.Monad.State
import Graphics.GLUtils

import Engine.System
import Engine.World
import Engine.Component.Component

import Types.Types
import Types.Color
import Utils.Utils
import Utils.Projection
import Utils.HashMap
import qualified Data.HashMap.Strict as HashMap
import Freecell.Utils
import Data.List
import Data.Ord
import Data.Maybe

import Graphics.Drawing
import Graphics.Rendering.OpenGL.Raw.Core31
import Data.IORef
import Freecell.Context.GameContext
import Math.Vector
import Utils.System
import FreeCell
import Debug.Trace

make :: SysMonad GameContext IO (System GameContext)
make = do
        fcgame <- liftIO $ newIORef empty
        initS fcgame

        return $ System (run fcgame)

clearGame fcgame = do
        putSysData fcgame SysData { game = Nothing }
        cds <- components gameContext cards
        deleteComponents systemContext drawStates
        deleteComponents systemContext mouseDrags
        deleteComponents systemContext selectables
        deleteComponents gameContext cards
        removeEntities (HashMap.keys cds)

pickSelectable pos (e, (Selectable size), card, (DrawState p)) =
        posInRect (v3tov2 pos) (Rect (v3tov2 p) size)

makeGameMove fcgame f = do
        sd@SysData { game = mgame } <- getSysData fcgame
        putSysData fcgame sd { game = fmap f mgame }

inputTouchUp' fcgame pos touchid = do
        sd@SysData { game = mgame } <- getSysData fcgame

        whenMaybe mgame $ \board -> do
            unproj <- doLerpUnproject pos (-5)
            comps <- zipComps2 (sysComps mouseDrags) (gameComps cards)
            let targetLocation = dropLocationForPos board (v3tov2 unproj)

            forM_ comps $ \(e, _, (UICard card _)) -> do
                removeComp sysCon e mouseDrags

                case moveCard board card targetLocation of
                    x | solvedBoard x -> do
                        runEventId gameCon wonGame
                        clearGame fcgame
                    x | null $ allPermissable x -> do
                        runEventId gameCon lostGame
                        clearGame fcgame
                    x | otherwise -> putSysData fcgame sd { game = Just x }

        return False

inputTouchDown' fcgame pos touchid = do
        SysData { game = mgame } <- getSysData fcgame
        whenMaybe mgame $ \game -> do
            unproj <- doLerpUnproject pos (-5)

            comps <- zipComps3 (sysComps selectables) (gameComps cards) (sysComps drawStates)

            let depth :: (e, Selectable, UICard, DrawState) -> Maybe Int
                depth (e, _, (UICard card _), _) = cardDepth game card
                selected = listToMaybe . sortOnMaybe depth . filter (pickSelectable unproj) $ comps

            whenMaybe selected $ (\(e, _, _, (DrawState p)) ->
                addComp systemContext e mouseDrags $ MouseDrag (p - unproj))

        return False

inputTouchLoc' pos touchid = do
        {-liftIO $ print $ "Input ping " ++ (show pos)-}
        return False

newGame' :: IORef SysData -> SysMonad GameContext IO ()
newGame' fcgame = do
        liftIO $ print "New Game"

        board <- liftIO $ makeGame

        mapM_ (\c -> spawnCard fcgame (v3 0 0 (-5)) c) allCards

        putSysData fcgame $ SysData $ Just board

spawnCard :: IORef SysData -> V3 -> Card -> SysMonad GameContext IO ()
spawnCard fcgame pos card = do
        let scale = 1

        e <- spawnEntity
        addComp systemContext e drawStates $ DrawState pos
        addComp systemContext e selectables $ Selectable (Size (0.726 * scale) scale)
        addComp gameContext e cards (UICard card nullTex)

        {- Let other systems know about the new entity -}
        RSC { _spawnedEntity } <- getRSC sysCon
        runEvent sysCon (\x -> x e) spawnedEntity

upDS delta board (e, (DrawState p), (UICard card _)) =
      let delta' = realToFrac delta 
          pilePos = posForCard board card in do
              md <- compForEnt systemContext e mouseDrags
              case md of
                  -- Only move toward pile if we're not dragging
                  Nothing -> return (e, (DrawState (movePos p pilePos 10 delta')))
                  _ -> return (e, (DrawState p))

run :: IORef SysData -> Double -> SysMonad GameContext IO ()
run fcgame delta = do
          SysData { game } <- getSysData fcgame
          whenMaybe game $ \board -> do
              {- Can't use normal upComps2 function because we want to
              - operate when a mouseDrag component is NOT present. -}
              ds <- components systemContext drawStates
              cds <- components gameContext cards
              putComps systemContext drawStates =<< mapM (upDS delta board) (zipHashes2 ds cds)

initS :: IORef SysData -> SysMonad GameContext IO ()
initS fcgame = do
        liftIO $ glClearColor 0 0.5 0 1
        registerEvent sysCon printAll (printSysData fcgame)
        registerEvent sysCon inputTouchUp (inputTouchUp' fcgame)
        registerEvent sysCon inputTouchDown (inputTouchDown' fcgame)
        registerEvent sysCon inputTouchLoc (inputTouchLoc')
        registerEvent gameCon newGame (newGame' fcgame)
        registerResource gameCon getGame $ fmap game (getSysData fcgame)

data SysData = SysData { game :: Maybe Board } deriving (Show)

empty = SysData Nothing
