{-# LANGUAGE NamedFieldPuns #-}

module Freecell.Systems.Game (make) where

import Control.Monad.State
import Graphics.GLUtils

import Engine.System
import Engine.World
import Engine.Component

import Types.Types
import Types.Color
import Utils.Utils
import Utils.Projection
import Utils.HashMap
import Data.List
import Data.Ord
import Data.Maybe

import Graphics.Drawing
import Graphics.Rendering.OpenGL.Raw.Core31
import Data.IORef
import Freecell.Context.Game
import Engine.GameContext
import Math.Vector
import Utils.System

make :: SysMonad EXGameContext IO (System EXGameContext)
make = do
        fcgame <- liftIO $ newIORef empty
        initS fcgame
        return $ System (run fcgame)

pickSelectable pos (e, (Selectable size), card, (DrawState p)) =
        posInRect (v3tov2 pos) (Rect (v3tov2 p) size)

inputTouchUp' pos touchid = do
        cs <- componentsAsList mouseDrags
        mapM_ (\(e, _) -> do
            removeComp e mouseDrags)
            cs
        return False

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = map snd . sortBy (comparing fst) . map (\x -> (f x, x))

inputTouchDown' fcgame pos touchid = do
        unproj <- doLerpUnproject pos (-5)

        ds <- components drawStates
        sel <- components selectables
        cds <- gameComponents cards

        let comps = zipHashes3 sel cds ds
            depth :: (e, Selectable, Card, DrawState) -> Int
            depth (e, _, (Card a), _) = 1
            selected = listToMaybe . sortOn depth . filter (pickSelectable unproj) $ comps

        whenMaybe selected $ (\(e, _, _, (DrawState p)) ->
            addComp e mouseDrags $ MouseDrag (p - unproj))

        {- Stacked version
        mapM_ (\(e, (DrawState p), _) -> do
            addComp e mouseDrags $ MouseDrag (p - unproj))
            selected
            -}

        return False

inputTouchLoc' pos touchid = do
        {-liftIO $ print $ "Input ping " ++ (show pos)-}
        return False

newGame' :: IORef SysData -> SysMonad EXGameContext IO ()
newGame' fcgame = do
        liftIO $ print "New Game"

        let stack1 = map Card [1..3]
            stack2 = map Card [4..6]

        mapM_ (\c -> spawnCard fcgame (v3 0 0 (-5)) c) stack1
        mapM_ (\c -> spawnCard fcgame (v3 2 0 (-5)) c) stack2

        putSysData fcgame $ SysData $ Just (FreecellGame stack1 stack2)

spawnCard fcgame pos card = do
        let scale = 2

        e <- spawnEntity
        addComp e drawStates $ DrawState pos
        addComp e selectables $ Selectable (Size (0.726 * scale) scale)
        addGameComp e cards card

run fcgame delta =
      do
          return ()

initS fcgame = do
        liftIO $ glClearColor 0 0.5 0 1
        registerEvent printAll (printSysData fcgame)
        registerEvent inputTouchUp (inputTouchUp')
        registerEvent inputTouchDown (inputTouchDown' fcgame)
        registerEvent inputTouchLoc (inputTouchLoc')
        registerGameEvent newGame (newGame' fcgame)

data FreecellGame = FreecellGame [Card] [Card] deriving Show

data SysData = SysData { game :: Maybe FreecellGame } deriving (Show)

empty = SysData Nothing
