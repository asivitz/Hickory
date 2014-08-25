{-# LANGUAGE NamedFieldPuns #-}

module Freecell.Systems.Menu (make) where

import Engine.System
import Engine.World
import Types.Types
import Types.Color
import Math.Vector

import Freecell.Context.Game
import Engine.GameContext

import Graphics.DrawText
import Menus.Menus
import Menus.Construction
import qualified Systems.DrawText as DrawText
import qualified Systems.Menus as Menus

make :: SysMonad EXGameContext IO (System c)
make = do
        RPC { _pushMenuScreen } <- getRPC
        gamerpc <- getGameRPC
        _pushMenuScreen (mainMenu gamerpc)
        return $ System nullRun

font = "goudy_bookletter_1911"

mainMenu :: GameRPC -> MenuScreen Scalar (EXEvent IO)
mainMenu GameRPC { _newGame } = MenuScreen [simpleMenuButton 0 "New Game" PopScreen _newGame] 0.5

type EXEvent m = Menus.MenuEvent EXGameContext m

simpleMenuButton :: Int -> String -> ScreenAction Scalar (EXEvent IO) -> [EXEvent IO] -> UIElement Scalar (EXEvent IO)
simpleMenuButton idx txt action events = UIElement (Just (Button (RRect (center 0, beg 40) (end 40, beg 30)) (events, Just action))) $ 
    MenuRenderSpec ([], [font], []) $ \(MenuResources _ [pid] _) ->
        \fraction incoming ->
            let frac' = constrainInterval fraction idx in
            [(beg (40 * (realToFrac (1 + idx))), center 0, 
                TextMenuDrawCommand pid DrawText.textcommand { text = txt, fontSize = 6, color = rgba 1 1 1 frac' })]
