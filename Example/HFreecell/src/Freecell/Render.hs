{-# LANGUAGE NamedFieldPuns #-}

module Freecell.Render (render, loadResources, view) where

import Data.List
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
import React.React

data Resources = Resources {
               vanillaShader :: Shader,
               blankCard :: TexID,
               cardTexes :: HashMap.HashMap Card TexID
               }

mkCard ::  Shader -> HashMap.HashMap Card TexID -> Card -> V3 -> Comp
mkCard shader cardTexHash card pos = let tid = HashMap.lookup card cardTexHash in
    case tid of
        Just t -> mkTerminal $ RSquare (Size 1 1) pos white t shader
        Nothing -> mkTerminal NoRender

data RenderTree = RSquare (Size Float) V3 Color TexID Shader
                | List [RenderTree]
                | NoRender
                deriving (Show)

rtDepth :: RenderTree -> Scalar
rtDepth (RSquare _ (Vector3 _ _ z) _ _ _) = z
rtDepth _ = 0

type Comp = Component RenderTree

view :: Resources -> Model -> Comp
view (Resources nillaSh blankTex cardTexHash) (Model _ board) = Stateless (List . map renderComp) (piles ++ cards)
        where piles = map (\pos -> mkTerminal $ RSquare (Size 1 1) (v2tov3 pos (-40)) white blankTex nillaSh) (drop 8 pilePositions)
              cards = map (\card -> let pilePos = posForCard board card in
                          mkCard nillaSh cardTexHash card pilePos) allCards

render :: Layer -> Comp -> IO ()
render layer comp = renderTree layer (renderComp comp)

renderTree :: Layer -> RenderTree -> IO ()
renderTree layer NoRender = return ()
renderTree layer (RSquare size pos color tex shader) = drawSpec pos layer (Square size color tex shader)
renderTree layer (List children) = mapM_ (renderTree layer) (sortOn rtDepth children)

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
