{-# LANGUAGE NamedFieldPuns #-}

module Systems.FreeCellMenu (make) where

import Engine.System
import Engine.World
import Types.Types
import Types.Color
import Math.Vector

import Context.Game

import Graphics.DrawText
import Menus.Menus
import Menus.Construction
import qualified Systems.Draw as Draw
import qualified Systems.DrawText as DrawText
import qualified Systems.Menus as Menus

make texes dt draw menus = System nullRun (initS dt texes draw menus)

font = "goudy_bookletter_1911"

initS dt texes draw menus = do
        Menus.pushScreen menus draw texes dt mainMenu

mainMenu :: MenuScreen Scalar EXGameContext
mainMenu = MenuScreen [simpleMenuButton 0 "New Game" PopScreen []]
                      0.5

simpleMenuButton :: Int -> String -> ScreenAction Scalar EXGameContext -> [SysMonad EXGameContext IO ()] -> UIElement Scalar EXGameContext
simpleMenuButton idx txt action events = UIElement (Just (Button (RRect (center 0, beg 40) (end 40, beg 30)) (events, Just action))) $ 
    MenuRenderSpec ([], [font], []) $ \(MenuResources _ [pid] _) ->
        \fraction incoming ->
            let frac' = constrainInterval fraction idx in
            [(beg (40 * (realToFrac (1 + idx))), center 0, 
                TextMenuDrawCommand pid DrawText.textcommand { text = txt, fontSize = 6, color = rgba 1 1 1 frac' })]
