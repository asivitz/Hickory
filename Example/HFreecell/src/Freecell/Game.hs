{-# LANGUAGE NamedFieldPuns #-}

module Freecell.Game where

import qualified Data.HashMap.Strict as HashMap
import Math.Vector
import System.Random
import Math.Matrix
import Types.Types
import FreeCell
import Freecell.Utils
import Engine.Scene.Input
import Data.Maybe
import Data.List
import Utils.Projection
import Layer.Layer

type ViewInfo = (Mat44, Size Int)

-- Game Layer
type Model = Board

data Msg = MoveCard Card Location

update :: Model -> Msg -> Model
update board (MoveCard card loc) = moveCard board card loc

gameLayer :: Double -> Layer Model Msg
gameLayer delta = foldl' update

-- UI Layer 
data UIState = UIState {
             sel :: Maybe Card,
             cursor :: V2,
             offset :: V2,
             cardPos :: HashMap.HashMap Card V3,
             randGen :: StdGen
             }

mkUI :: Model -> StdGen -> UIState
mkUI model stdgen = UIState {
                            sel = Nothing,
                            cursor = vZero,
                            offset = vZero,
                            cardPos = HashMap.empty,
                            randGen = stdgen
                            }

uiLayer :: (Double, ViewInfo) -> Layer (UIState, Model) RawInput
uiLayer input = constructLayer (uiInput input) (stepUI input) (gameLayer (fst input))

{-
                 x | solvedBoard x -> (x, [Won])
                 x | null $ allPermissable x -> (x, [Lost])
                 x | otherwise -> (x, [])
                 -}

uiInput :: (Double, ViewInfo) -> Model -> UIState -> RawInput -> (UIState, [Msg])
uiInput (_, (mat, ss))
        board
        ui@UIState { sel, cursor, offset, cardPos = cardPosMap }
        input =
        case input of
            InputTouchesLoc [(pos, _)] -> (ui { cursor = pos }, [])
            InputTouchesDown [(pos, _)] -> let cursorPos = unproject pos (-5) mat ss :: V3
                                               predicate :: Card -> Maybe (V3, Card)
                                               predicate c = let homePos = posForCard board c in
                                                   if v3tov2 cursorPos `posInRect` Rect (v3tov2 homePos) (Size 0.66 1)
                                                       then Just (homePos, c)
                                                       else Nothing
                                               card = listToMaybe $ reverse $ sortOn (\(Vector3 _ _ z, _) -> z) $ mapMaybe predicate allCards
                                               offset' = case card of
                                                            Just (p, _) -> v3tov2 (p - cursorPos)
                                                            Nothing -> vZero
                                               in (ui { sel = fmap snd card, cursor = pos, offset = offset' }, [])
            InputTouchesUp [(_, pos, _)] -> case sel of
                                                Just c -> let cursorPos = unproject pos (-5) mat ss :: V3
                                                              cardLoc = v3tov2 cursorPos + offset
                                                              targetLocation = dropLocationForPos board cardLoc in
                                                                  (ui { sel = Nothing, cursor = pos, cardPos = HashMap.insert c (v2tov3 cardLoc (-5)) cardPosMap }, [MoveCard c targetLocation])
                                                Nothing -> (ui { sel = Nothing, cursor = pos }, [])
            _ -> (ui, [])

stepUI :: (Double, ViewInfo) -> Model -> UIState -> UIState
stepUI (delta, _) board ui@UIState { cardPos } = ui { cardPos = foldl' updateCardPos cardPos allCards }
    where updateCardPos cmap card = let homePos = posForCard board card
                                        Vector3 _ _ z = homePos
                                        p = fromMaybe homePos (HashMap.lookup card cmap)
                                        p' = movePos (v3tov2 p) (v3tov2 homePos) 20 delta
                                        in HashMap.insert card (v2tov3 p' z) cmap

