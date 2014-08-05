module Systems.Menus where

import Math.Vector
import Types.Types
import Engine.Event
import Engine.System

import Control.Monad.State

data MenuScreen = MenuScreen [UIElement] Scalar

data ScreenAction = PushScreen MenuScreen
                  | RefreshScreen
                  | PopScreen

data Button = Button Rect (Maybe Event, Maybe ScreenAction)

type DrawFunc = IO ()

data UIElement = UIElement DrawFunc (Maybe Button)

posInRect (Vector2 px py) (Rect (Vector2 ox oy) (Size w h)) =
        ((abs (ox - px)) < (w/2)) && ((abs (oy - py)) < (h/2))

data SysData = SysData [MenuScreen]

empty = SysData []

make menus = System (run menus) nullHandleEvent nullInit

run menus delta = do
        SysData navstack <- getSysData menus
        liftIO $ mapM_ (\(MenuScreen elements duration) -> mapM_ renderElement elements) navstack

renderElement (UIElement drawfunc _) = drawfunc
