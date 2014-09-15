{-# LANGUAGE NamedFieldPuns #-}

module Freecell.Render (render, loadResources) where

import FreeCell
import Freecell.Utils
import Graphics.Drawing
import Graphics.GLUtils
import Systems.Textures
import Utils.Utils
import Engine.Component.CompUtils
import Engine.Component.Component
import Engine.Component.Model
import Engine.Component.Entity
import Types.Color
import Freecell.Component
import Systems.Textures
import Types.Types
import Systems.Draw
import qualified Data.HashMap.Strict as HashMap
import Data.Foldable (foldlM)
import Utils.HashMap
import Data.List
import Data.Ord
import Data.Maybe

data Resources = Resources {
               vanillaShader :: Maybe Shader,
               blankCard :: Maybe TexID,
               cardTexes :: HashMap.HashMap Card TexID
               }

drawCard ::  Shader -> HashMap.HashMap Card TexID -> (e, Card, DrawState) -> IO ()
drawCard shader cardTexHash (_, card, (DrawState pos)) = do
        let tid = HashMap.lookup card cardTexHash
        whenMaybe tid $ \t -> do
            let spec = Square (Size 1 1) white t shader
            drawSpec pos uiLabel spec

depth :: Board -> EntHash MouseDrag -> (Entity, Card, DrawState) -> Maybe Int
depth board mouseDragHash (e, card, _) = 
        case (HashMap.lookup e mouseDragHash) of
            Nothing -> cardDepth board card
            _ -> Just (-2)

render :: Resources -> Model ComponentStore GameModel -> IO ()
render (Resources nillaSh blankTex cardTexHash) model = do
        {-print $ "Rendering model: " ++ (show model)-}

        {-whenMaybe2 nillaSh blankTex $ \sh tid -> do-}
            {-let ds = getModelComponents drawStates model-}
            {-forM_ (stripEnts ds) $ \(DrawState pos) ->-}

        whenMaybe nillaSh $ \sh -> do
                let mds = getModelComponents mouseDrags model
                    ds = getModelComponents drawStates model
                    cds = getModelComponents cardComps model
                    board = getBoard model
                    sorted = map snd . sortBy (comparing fst) . mapMaybe (\x -> 
                                              case depth board mds x of 
                                                                        Nothing -> Nothing 
                                                                        Just a -> Just (a, x))
                                                                        $ zipHashes2 cds ds

                mapM_ (drawCard sh cardTexHash) sorted

{-cardNumber (Card rk st) = (suitIndexOffset st) + (rankIndex rk)-}

{-cardImagePath pre t = "PlayingCards/cards/" ++ pre ++ "_" ++ t ++ ".png"-}
{-allRanks = ["A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"]-}
{-texes = [cardImagePath st rk | st <- ["sp", "he", "di", "cl"], rk <- allRanks]-}

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
        return $ Resources nilla blank texes

{-
texForCard draw card = do
        SysData { cardTexes } <- getSysData draw
        return $ cardTexes !! cardNumber card

drawCard ::  Shader -> (e, UICard, DrawState) -> SysMonad c IO ()
drawCard shader (_, (UICard i texid), (DrawState pos)) = do
        let spec = Square (Size 1 1) white texid shader
        Draw.drawSpec pos worldLabel spec

depth :: Board -> HashMap.HashMap Entity MouseDrag -> (Entity, UICard, DrawState) -> Maybe Int
depth board mouseDragHash (e, (UICard card _), _) = 
        if isJust (HashMap.lookup e mouseDragHash) 
            then Just (-2)
            else cardDepth board card

drawPile shader tex pos = do
        liftIO $ Draw.drawSpec (v2tov3 pos (-40)) worldLabel (Square (Size 1 1) white tex shader)
        -}
