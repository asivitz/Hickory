{-# LANGUAGE NamedFieldPuns #-}

module Freecell.Systems.Draw (make) where

import Control.Monad.State
import Graphics.GLUtils

import Engine.System
import Engine.World
import Engine.Component
import Engine.Entity

import Types.Types
import Types.Color
import Utils.Utils
import Utils.Projection
import Camera.Camera
import Utils.HashMap
import Freecell.Utils
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Ord
import Data.List

import Graphics.Drawing
import Graphics.Rendering.OpenGL.Raw.Core31
import Data.IORef
import Freecell.Context.Game
import Engine.GameContext
import Math.Vector
import Utils.System
import qualified Systems.Draw as Draw

make :: SysMonad EXGameContext IO (System EXGameContext)
make = do
        draw <- liftIO $ newIORef empty
        initS draw
        return $ System (run draw)

cardTex = "card.png"

spawnedEnt draw e = do
        whenMaybeM (gameCompForEnt e cards) $ \(Card i _) -> do
            SysData { cardTexes } <- getSysData draw
            addGameComp e cards (Card i (cardTexes !! (i - 1)))

drawCard ::  Shader -> (e, Card, DrawState) -> SysMonad c IO ()
drawCard shader (_, (Card i texid), (DrawState pos)) = do
        let spec = Square (Size 2 2) white texid shader
        liftIO $ Draw.drawSpec pos worldLabel spec

depth :: FreecellGame -> HashMap.HashMap Entity MouseDrag -> (Entity, Card, DrawState) -> Maybe Int
depth game mouseDragHash (e, card, _) = 
        if isJust (HashMap.lookup e mouseDragHash) 
            then Just (-2)
            else cardDepth game card

run draw delta =
      do
          GameRPC { _getGame } <- getGameRPC
          mgame <- _getGame
          whenMaybe mgame $ \game -> do
            SysData { vanilla } <- getSysData draw
            whenMaybe vanilla $ \sh -> do
                mds <- components mouseDrags
                ds <- components drawStates
                cds <- gameComponents cards

                let sorted = map snd . sortBy (comparing fst) . mapMaybe (\x -> 
                                                   case depth game mds x of 
                                                                   Nothing -> Nothing 
                                                                   Just a -> Just (a, x))
                                        $ zipHashes2 cds ds

                mapM_ (drawCard sh) sorted
            return ()

cardImagePath pre t = "cards/" ++ pre ++ "_" ++ t ++ ".png"
texes = map (cardImagePath "sp") ["A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"]

initS draw = do
        liftIO $ glClearColor 0 0.5 0 1

        registerEvent spawnedEntity (spawnedEnt draw)

        RPC { _reserveTex, _reserveShader, _setWorldProjection } <- getRPC

        _setWorldProjection (Ortho 10 1 100)

        card_texes <- mapM _reserveTex texes

        nilla <- _reserveShader ("Shader.vsh", "Shader.fsh")

        putSysData draw SysData { cardTexes = catMaybes card_texes, vanilla = nilla }

data SysData = SysData { cardTexes :: [TexID], vanilla :: Maybe Shader } deriving (Show)

empty = SysData [] Nothing

