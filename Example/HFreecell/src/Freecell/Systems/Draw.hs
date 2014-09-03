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
import Freecell.Context.GameContext
import Math.Vector
import Utils.System
import qualified Systems.Draw as Draw
import FreeCell

make :: SysMonad EXGameContext IO (System EXGameContext)
make = do
        draw <- liftIO $ newIORef empty
        initS draw
        return $ System (run draw)

suitIndexOffset Spade = 0
suitIndexOffset Heart = 13
suitIndexOffset Diamond = 26
suitIndexOffset Club = 39
rankIndex Ace = 0
rankIndex Two = 1
rankIndex Three = 2
rankIndex Four = 3
rankIndex Five = 4
rankIndex Six = 5
rankIndex Seven = 6
rankIndex Eight = 7
rankIndex Nine = 8
rankIndex Ten = 9
rankIndex Jack = 10
rankIndex Queen = 11
rankIndex King = 12

texForCard draw (Card rk st) = do
        SysData { cardTexes } <- getSysData draw
        return $ cardTexes !! ((suitIndexOffset st) + (rankIndex rk))

spawnedEnt draw e = do
        whenMaybeM (compForEnt gameContext e cards) $ \(UICard card _) -> do
            tid <- texForCard draw card
            addComp gameContext e cards (UICard card tid)

drawCard ::  Shader -> (e, UICard, DrawState) -> SysMonad c IO ()
drawCard shader (_, (UICard i texid), (DrawState pos)) = do
        let spec = Square (Size 1 1) white texid shader
        liftIO $ Draw.drawSpec pos worldLabel spec

depth :: Board -> HashMap.HashMap Entity MouseDrag -> (Entity, UICard, DrawState) -> Maybe Int
depth board mouseDragHash (e, (UICard card _), _) = 
        if isJust (HashMap.lookup e mouseDragHash) 
            then Just (-2)
            else cardDepth board card

drawPile shader tex pos = do
        liftIO $ Draw.drawSpec (v2tov3 pos (-40)) worldLabel (Square (Size 1 1) white tex shader)

run draw delta =
      do
          GameRPC { _getGame } <- getRPC gameCon
          mgame <- _getGame
          whenMaybe mgame $ \game -> do
            SysData { vanilla, blankTex } <- getSysData draw
            whenMaybe2 vanilla blankTex $ \sh bl -> do
                forM_ (drop 8 pilePositions) (drawPile sh bl)

                mds <- components sysCon mouseDrags
                ds <- components sysCon drawStates
                cds <- components gameCon cards

                let sorted = map snd . sortBy (comparing fst) . mapMaybe (\x -> 
                                                   case depth game mds x of 
                                                                   Nothing -> Nothing 
                                                                   Just a -> Just (a, x))
                                        $ zipHashes2 cds ds

                mapM_ (drawCard sh) sorted
            return ()

cardImagePath pre t = "PlayingCards/cards/" ++ pre ++ "_" ++ t ++ ".png"
allRanks = ["A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"]
texes = [cardImagePath st rk | st <- ["sp", "he", "di", "cl"], rk <- allRanks]

initS draw = do
        liftIO $ glClearColor 0 0.5 0 1

        registerEvent sysCon spawnedEntity (spawnedEnt draw)

        RPC { _reserveTex, _reserveShader, _setWorldProjection } <- getRPC sysCon

        _setWorldProjection (Ortho 10 1 100)

        card_texes <- mapM _reserveTex texes

        blank <- _reserveTex "PlayingCards/blank.png"

        nilla <- _reserveShader ("Shader.vsh", "Shader.fsh")

        putSysData draw SysData { cardTexes = catMaybes card_texes, blankTex = blank, vanilla = nilla }

data SysData = SysData { cardTexes :: [TexID], blankTex :: Maybe TexID, vanilla :: Maybe Shader } deriving (Show)

empty = SysData [] Nothing Nothing

