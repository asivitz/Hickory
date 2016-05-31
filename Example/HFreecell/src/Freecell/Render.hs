{-# LANGUAGE NamedFieldPuns #-}

module Freecell.Render (render, loadResources) where

import Data.Maybe
import FreeCell
import Freecell.Utils
import Graphics.Drawing
import Graphics.Shader
import Graphics.DrawUtils
import Utils.Utils
import Math.Matrix
import Types.Color
import Freecell.Component
import Types.Types
import qualified Data.HashMap.Strict as HashMap
import Data.Foldable (foldlM)
import Control.Monad
import Math.Vector
import Graphics.Rendering.OpenGL.Raw.Core31
import Textures.Textures

data Resources = Resources {
               vanillaShader :: Shader,
               blankCard :: TexID,
               cardTexes :: HashMap.HashMap Card TexID
               }

drawCard ::  Shader -> HashMap.HashMap Card TexID -> Layer -> Card -> V3 -> IO ()
drawCard shader cardTexHash layer card pos = do
        let tid = HashMap.lookup card cardTexHash
        whenMaybe tid $ \t -> do
            let spec = Square (Size 1 1) white t shader
            drawSpec pos layer spec

{-
depth :: Board -> EntHash MouseDrag -> (Entity, Card, DrawState) -> Maybe Int
depth board mouseDragHash (e, card, _) = 
        case HashMap.lookup e mouseDragHash of
            Nothing -> cardDepth board card
            _ -> Just (-2)
            -}

drawPile shader tex layer pos = do
        drawSpec (v2tov3 pos (-40)) layer (Square (Size 1 1) white tex shader)

render :: Resources -> Layer -> Model -> IO ()
render (Resources nillaSh blankTex cardTexHash) layer (Model _ board) = do
        {-whenMaybe2 nillaSh blankTex $ \sh tid -> do-}
        forM_ (drop 8 pilePositions) (drawPile nillaSh blankTex layer)


        forM_ allCards $ \card -> do
            let pilePos = posForCard board card 
            drawCard nillaSh cardTexHash worldLayer card pilePos

        {-whenMaybe nillaSh $ \sh -> do-}
        return ()
            {-
                let mds = getModelComponents mouseDrags model
                    ds = getModelComponents drawStates model
                    cds = getModelComponents cardComps model
                    board = getBoard model
                    sorted = map snd . sortBy (comparing fst) . mapMaybe (\x -> 
                                              case depth board mds x of 
                                                                        Nothing -> Nothing 
                                                                        Just a -> Just (a, x))
                                                                        $ zipHashes2 cds ds

                mapM_ (drawCard sh cardTexHash layer) sorted
                -}

rankSymbol rk = case rk of
                    Ace -> "A"
                    Two -> "2"
                    Three -> "3"
                    Four -> "4"
                    Five -> "5"
                    Six -> "6"
                    Seven -> "7"
                    Eight -> "8"
                    Nine -> "9"
                    Ten -> "10"
                    Jack -> "J"
                    Queen -> "Q"
                    King -> "K"

suitSymbol st = case st of
                    Heart -> "he"
                    Spade -> "sp"
                    Diamond -> "di"
                    Club -> "cl"

cardTexPath (Card rk st) = let r = rankSymbol rk
                               s = suitSymbol st in
                                   "PlayingCards/cards/" ++ s ++ "_" ++ r ++ ".png"

loadResources :: String -> IO Resources
loadResources path = do
        glClearColor 0.3 0.5 0.1 1
        
        {-card_texes <- mapM loadTexture texes-}

        texes <- foldlM (\hash card -> do
               tid <- loadTexture path (cardTexPath card)
               case tid of
                   Nothing -> return hash
                   Just t -> return $ HashMap.insert card t hash)
               HashMap.empty
               allCards

        blank <- loadTexture path "PlayingCards/blank.png"

        nilla <- loadShader path "Shader.vsh" "Shader.fsh"
        {-solid <- loadShader path "Shader.vsh" "SolidColor.fsh"-}
        return $ Resources (fromJust nilla) (fromJust blank) texes
