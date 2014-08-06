module Menus.Menus where

import Types.Types
import Engine.Event
import Graphics.Drawing
import Graphics.GLUtils
import Graphics.DrawText
import Math.Vector
import Types.Color

data Button = Button Rect (Maybe Event, Maybe ScreenAction)

data MenuDrawCommand = SquareMenuDrawCommand (RelativePos Float Int, RelativePos Float Int) Color (Maybe TexID) Shader
                     | TextMenuDrawCommand PrinterID TextCommand

data MenuResources = MenuResources [TexID] [PrinterID] [Shader]

type MenuResourceNames = ([String], [String], [(String, String)])

type UIRender a = [(RelativePos Scalar a, RelativePos Scalar a, MenuDrawCommand)]

data MenuRenderSpec = MenuRenderSpec MenuResourceNames (MenuResources -> UIRender Int)

data UIElement = UIElement (Maybe Button) MenuRenderSpec

data ResolvedUIElement = ResolvedUIElement (Maybe Button) (UIRender Int)

data MenuScreen = MenuScreen [UIElement] Scalar

data ResolvedMenuScreen = ResolvedMenuScreen [ResolvedUIElement] Scalar

data ScreenAction = PushScreen MenuScreen
                  | RefreshScreen
                  | PopScreen

makeLabel :: MenuRenderSpec -> UIElement
makeLabel s = UIElement Nothing s
