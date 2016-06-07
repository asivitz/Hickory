{-# LANGUAGE NamedFieldPuns #-}

module Freecell.Render (Msg, update, render, loadResources, RenderTree(..), mkUI, renderTree, uiInput, stepUI, updateUI, Model, UIState, Resources) where

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
import System.Random

data Resources = Resources {
               vanillaShader :: Shader,
               blankCard :: TexID,
               cardTexes :: HashMap.HashMap Card TexID
               }

data Msg = MoveCard Card Location

data UIMsg = Lost | Won | NewGame

type Model = Board

data UIState = UIState {
             sel :: Maybe Card,
             cursor :: V2,
             offset :: V2,
             cardPos :: HashMap.HashMap Card V3,
             lastUIMsg :: Maybe UIMsg,
             randGen :: StdGen
             }

mkUI :: Model -> StdGen -> UIState
mkUI model stdgen = UIState {
                            sel = Nothing,
                            cursor = vZero,
                            offset = vZero,
                            cardPos = HashMap.empty,
                            lastUIMsg = Nothing,
                            randGen = stdgen
                            }

type ViewInfo = (Mat44, Size Int)

uiInput :: ViewInfo -> Model -> UIState -> RawInput -> (UIState, [Msg])
uiInput (mat, ss)
        board
        ui@UIState { sel, cursor, offset, cardPos = cardPosMap, lastUIMsg }
        input =
        case input of
            InputTouchesLoc [(pos, _)] -> (ui { cursor = pos }, [])
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
                                               in (ui { sel = fmap snd card, cursor = pos, offset = offset }, [])
            InputTouchesUp [(_, pos, _)] -> case sel of
                                                Just c -> let cursorPos = unproject pos (-5) mat ss :: V3
                                                              cardLoc = v3tov2 cursorPos + offset
                                                              targetLocation = dropLocationForPos board cardLoc in
                                                                  (ui { sel = Nothing, cursor = pos, cardPos = HashMap.insert c (v2tov3 cardLoc (-5)) cardPosMap }, [MoveCard c targetLocation])
                                                Nothing -> (ui { sel = Nothing, cursor = pos }, [])
            _ -> (ui, [])

update :: Model -> Msg -> (Model, [UIMsg])
update board (MoveCard card loc) =
             case moveCard board card loc of
                 x | solvedBoard x -> (x, [Won])
                 x | null $ allPermissable x -> (x, [Lost])
                 x | otherwise -> (x, [])


stepUI :: Model -> Double -> UIState -> UIState
stepUI board delta ui@UIState { cardPos } = ui { cardPos = foldl' updateCardPos cardPos allCards }
    where updateCardPos map card = let homePos = posForCard board card
                                       Vector3 x y z = homePos
                                       p = fromMaybe homePos (HashMap.lookup card map)
                                       p' = movePos (v3tov2 p) (v3tov2 homePos) 20 delta
                                       in HashMap.insert card (v2tov3 p' z) map

updateUI :: Model -> UIState -> UIMsg -> UIState
updateUI board ui uimsg =
        case uimsg of
            Won -> ui { lastUIMsg = Just uimsg }
            Lost -> ui { lastUIMsg = Just uimsg }

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
