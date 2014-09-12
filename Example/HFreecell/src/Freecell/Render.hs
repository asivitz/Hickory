module Freecell.Render where

import FreeCell
import Freecell.Utils
import Graphics.Drawing
import Graphics.GLUtils
import Systems.Textures
import Utils.Utils
import Engine.Component.CompUtils
import Engine.Component.Component
import Engine.Component.Model
import Types.Color
import Freecell.Component
import Control.Monad
import Systems.Textures
import Types.Types
import Systems.Draw

data Resources = Resources {
               vanillaShader :: Maybe Shader,
               blankCard :: Maybe TexID
               }

render :: Resources -> Model ComponentStore GameModel -> IO ()
render (Resources nillaSh blankTex) model = do
        {-print $ "Rendering model: " ++ (show model)-}

        whenMaybe2 nillaSh blankTex $ \sh tid -> do
            let ds = getModelComponents drawStates model
            forM_ (stripEnts ds) $ \(DrawState pos) ->
                drawSpec pos uiLabel (Square (Size 0.726 1) white tid sh)



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

cardNumber (Card rk st) = (suitIndexOffset st) + (rankIndex rk)

cardImagePath pre t = "PlayingCards/cards/" ++ pre ++ "_" ++ t ++ ".png"
allRanks = ["A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"]
texes = [cardImagePath st rk | st <- ["sp", "he", "di", "cl"], rk <- allRanks]


loadResources :: String -> IO Resources
loadResources path = do
        {-card_texes <- mapM loadTexture texes-}

        blank <- loadTexture path "PlayingCards/blank.png"

        nilla <- loadShader path "Shader.vsh" "Shader.fsh"
        {-solid <- loadShader path "Shader.vsh" "SolidColor.fsh"-}
        return $ Resources nilla blank

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
