{-# LANGUAGE NamedFieldPuns #-}

module Freecell.Render (render, loadResources, RenderTree(..), mkUI, renderTree, uiInput) where

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

data UIState = UIState {
             sel :: Maybe Card,
             cursor :: V2,
             offset :: V2
             }

mkUI :: Model -> UIState
mkUI model = UIState Nothing vZero vZero

uiInput :: Mat44 -> Size Int -> Model -> UIState -> RawInput -> UIState
uiInput mat ss (Model _ board) (UIState sel cursor offset) input =
        case input of
            InputTouchesLoc [(pos, _)] -> UIState sel pos offset
            InputTouchesDown [(pos, _)] -> let cursorPos = unproject pos (-5) mat ss :: V3
                                               predicate :: Card -> Maybe (V3, Card)
                                               predicate c = let homePos = posForCard board c in
                                                   if v3tov2 cursorPos `posInRect` Rect (v3tov2 homePos) (Size 0.66 1)
                                                       then Just (homePos, c)
                                                       else Nothing
                                               card = listToMaybe $ reverse $ sortOn (\(Vector3 _ _ z, c) -> z) $ mapMaybe predicate allCards
                                               offset = case card of
                                                            Just (p, c) -> v3tov2 (p - cursorPos)
                                                            Nothing -> vZero
                                               in UIState (fmap snd card) pos offset
            InputTouchesUp [(_, pos, _)] -> UIState Nothing pos offset
            _ -> UIState sel cursor offset

render :: Resources -> Mat44 -> Size Int -> Model -> UIState -> RenderTree
render (Resources nillaSh blankTex cardTexHash) mat ss (Model _ board) (UIState sel cursor offset) = List (piles ++ cards)
    where cursorPos = unproject cursor (-5) mat ss :: V3
          renderCard card = let tid = HashMap.lookup card cardTexHash
                                homePos = posForCard board card
                                pos = case sel of
                                          Just c -> if c == card
                                                        then if cardDepth board c == Just 0 then cursorPos + (v2tov3 offset 0) else v2tov3 (v3tov2 homePos) (-5)
                                                        else homePos
                                          Nothing -> homePos
                                in RSquare (Size 1 1) pos white (fromJust tid) nillaSh
          cards = map renderCard allCards
          piles = map (\pos -> RSquare (Size 1 1) (v2tov3 pos (-40)) white blankTex nillaSh) (drop 8 pilePositions)

data RenderTree = RSquare (Size Float) V3 Color TexID Shader
                | List [RenderTree]
                | NoRender
                deriving (Show)

rtDepth :: RenderTree -> Scalar
rtDepth (RSquare _ (Vector3 _ _ z) _ _ _) = z
rtDepth _ = 0

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
