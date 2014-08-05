{-# LANGUAGE NamedFieldPuns #-}

module Systems.Menus where

import Math.Vector
import Types.Types
import Engine.Event
import Engine.System

import Control.Monad.State

import qualified Systems.Draw as Draw

data MenuScreen = MenuScreen [UIElement] Scalar

data ScreenAction = PushScreen MenuScreen
                  | RefreshScreen
                  | PopScreen

data Button = Button Rect (Maybe Event, Maybe ScreenAction)

type DrawFunc = Size Int -> SysMonad IO ()

data UIElement = UIElement (Maybe Button) DrawFunc

makeLabel :: DrawFunc -> UIElement
makeLabel f = UIElement Nothing f

posInRect (Vector2 px py) (Rect (Vector2 ox oy) (Size w h)) =
        ((abs (ox - px)) < (w/2)) && ((abs (oy - py)) < (h/2))

data SysData = SysData [MenuScreen]

empty = SysData []

make menus draw = System (run menus draw) nullHandleEvent nullInit

run menus draw delta = do
        SysData navstack <- getSysData menus
        Draw.SysData { Draw.screenSize } <- getSysData draw
        mapM_ (\(MenuScreen elements duration) -> mapM_ (\(UIElement _ df) -> df screenSize) elements) navstack

pushScreen menus screen = do
        SysData navstack <- getSysData menus
        putSysData menus (SysData (screen:navstack))

