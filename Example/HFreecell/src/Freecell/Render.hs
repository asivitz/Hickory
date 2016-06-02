{-# LANGUAGE NamedFieldPuns #-}

module Freecell.Render (render, loadResources, view, Comp, RenderTree(..)) where

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
import Data.Dynamic
import Engine.Scene.Input
import Utils.Projection
import Debug.Trace

data Resources = Resources {
               vanillaShader :: Shader,
               blankCard :: TexID,
               cardTexes :: HashMap.HashMap Card TexID
               }

data CardState = CardState Bool V3
               deriving (Show)

mkCard ::  Shader -> HashMap.HashMap Card TexID -> Mat44 -> Size Int -> Card -> CardState -> Comp
mkCard shader cardTexHash mat ss card state = let tid = HashMap.lookup card cardTexHash in
    case tid of
        Just t -> Stateful id
                           (toDyn state)
                           (\s _ -> let CardState _ p = conv s in RSquare (Size 1 1) p white t shader)
                           []
                           (\i s -> toDyn $ cardInputFunc mat ss i (conv s))
                           (\x -> show (conv x :: CardState))
        Nothing -> mkTerminal NoRender

cardInputFunc :: Mat44 -> Size Int -> RawInput -> CardState -> CardState
cardInputFunc mat ss input s@(CardState sel oldPos) =
        case input of
            InputTouchesLoc [(pos, _)] -> let cursorPos = unproject pos (-5) mat ss :: V3 in
                if sel then CardState True cursorPos else s
            InputTouchesDown [(pos, _)] -> let cursorPos = unproject pos (-5) mat ss :: V3 in
                if v3tov2 cursorPos `posInRect` Rect (v3tov2 oldPos) (Size 1 1)
                    then CardState True cursorPos
                    else s
            InputTouchesUp [(_, pos, _)] ->
                if sel
                    then CardState False oldPos
                    else s
            _ -> s

data RenderTree = RSquare (Size Float) V3 Color TexID Shader
                | List [RenderTree]
                | NoRender
                deriving (Show)

rtDepth :: RenderTree -> Scalar
rtDepth (RSquare _ (Vector3 _ _ z) _ _ _) = z
rtDepth _ = 0

type Comp = Component RenderTree RawInput

view :: Resources -> Mat44 -> Size Int -> Model -> Comp
view (Resources nillaSh blankTex cardTexHash) mat ss (Model _ board) = Stateless (List . map renderComp) (piles ++ cards)
        where piles = map (\pos -> mkTerminal $ RSquare (Size 1 1) (v2tov3 pos (-40)) white blankTex nillaSh) (drop 8 pilePositions)
              cards = map (\card -> let pilePos = posForCard board card in
                          mkCard nillaSh cardTexHash mat ss card (CardState False pilePos)) allCards

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
               tid <- loadTexture' path (cardTexPath card)
               return $ HashMap.insert card tid hash)
               HashMap.empty
               allCards

        blank <- loadTexture' path "PlayingCards/blank.png"

        nilla <- loadShader' path "Shader.vsh" "Shader.fsh"
        {-solid <- loadShader path "Shader.vsh" "SolidColor.fsh"-}
        return $ Resources nilla blank texes
