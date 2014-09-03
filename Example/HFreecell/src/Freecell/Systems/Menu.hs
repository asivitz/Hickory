{-# LANGUAGE NamedFieldPuns #-}

module Freecell.Systems.Menu (make) where

import Engine.System
import Engine.World
import Types.Types
import Types.Color
import Math.Vector

import Freecell.Context.GameContext

import Graphics.DrawText
import Menus.Menus
import Menus.Construction
import qualified Systems.DrawText as DrawText
import qualified Systems.Menus as Menus

make :: SysMonad GameContext IO (System c)
make = do
        RPC { _pushMenuScreen } <- getRPC sysCon
        gamerpc <- getRPC gameCon
        _pushMenuScreen (mainMenu gamerpc)
        registerEvent gameCon lostGame $ do
            _pushMenuScreen (mainMenu gamerpc)
        registerEvent gameCon wonGame $ do
            _pushMenuScreen (mainMenu gamerpc)
        return $ System nullRun

font = "goudy_bookletter_1911"

mainMenu :: GameRPC -> MenuScreen Scalar (GameEvent IO)
mainMenu GameRPC { _newGame } = MenuScreen [simpleMenuButton 0 "New Game" PopScreen _newGame] 0.5

type GameEvent m = Menus.MenuEvent GameContext m

simpleMenuButton :: Int -> String -> ScreenAction Scalar (GameEvent IO) -> [GameEvent IO] -> UIElement Scalar (GameEvent IO)
simpleMenuButton idx txt action events = UIElement (Just (Button (RRect (center 0, beg 40) (end 40, beg 30)) (events, Just action))) $ 
    MenuRenderSpec ([], [font], []) $ \(MenuResources _ [pid] _) ->
        \fraction incoming ->
            let frac' = constrainInterval fraction idx in
            [(beg (40 * (realToFrac (1 + idx))), center 0, 
                TextMenuDrawCommand pid DrawText.textcommand { text = txt, fontSize = 6, color = rgba 1 1 1 frac' })]
