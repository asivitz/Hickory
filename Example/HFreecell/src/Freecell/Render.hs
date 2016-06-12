{-# LANGUAGE NamedFieldPuns #-}

module Freecell.Render where

import Data.List
import Data.Maybe
import FreeCell
import Freecell.Utils
import Graphics.Drawing
import Graphics.Shader
import Graphics.DrawUtils
import Types.Color
import Types.Types
import qualified Data.HashMap.Strict as HashMap
import Data.Foldable (foldlM)
import Math.Vector
import Graphics.Rendering.OpenGL.Raw.Core31
import Textures.Textures
import Utils.Projection
import Freecell.Game

data Resources = Resources {
               vanillaShader :: Shader,
               blankCard :: TexID,
               cardTexes :: HashMap.HashMap Card TexID
               }

render :: Resources -> ViewInfo -> Model -> UIState -> RenderTree
render (Resources nillaSh blankTex cardTexHash)
       (mat, ss)
       board
       UIState { sel, cursor, offset, cardPos = cardPosMap }
       = List (piles ++ cards)
    where cursorPos = unproject cursor (-5) mat ss :: V3
          renderCard card = let tid = HashMap.lookup card cardTexHash
                                homePos = posForCard board card
                                cardPos = fromMaybe homePos (HashMap.lookup card cardPosMap)
                                pos = case sel of
                                          Just c -> if c == card
                                                        then if cardDepth board c == Just 0 then cursorPos + v2tov3 offset 0 else v2tov3 (v3tov2 cardPos) (-5)
                                                        else cardPos
                                          Nothing -> cardPos
                                in RSquare (sizePosMat (Size 1 1) pos) white tid nillaSh
          cards = map renderCard allCards
          piles = map (\pos -> RSquare (sizePosMat (Size 1 1) (v2tov3 pos (-40))) white (Just blankTex) nillaSh) (drop 8 pilePositions)

rankSymbol :: Rank -> String
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

suitSymbol :: Suit -> String
suitSymbol st = case st of
                    Heart -> "he"
                    Spade -> "sp"
                    Diamond -> "di"
                    Club -> "cl"

cardTexPath :: Card -> String
cardTexPath (Card rk st) = let r = rankSymbol rk
                               s = suitSymbol st in
                                   "PlayingCards/cards/" ++ s ++ "_" ++ r ++ ".png"

loadResources :: String -> IO Resources
loadResources path = do
        glClearColor 0.3 0.5 0.1 1
        
        texes <- foldlM (\hash card -> do
               tid <- loadTexture' path (cardTexPath card)
               return $ HashMap.insert card tid hash)
               HashMap.empty
               allCards

        blank <- loadTexture' path "PlayingCards/blank.png"

        nilla <- loadShader' path "Shader.vsh" "Shader.fsh"
        {-solid <- loadShader path "Shader.vsh" "SolidColor.fsh"-}
        return $ Resources nilla blank texes
