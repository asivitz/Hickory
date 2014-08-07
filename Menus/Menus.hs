module Menus.Menus where

import Types.Types
import Engine.Event
import Graphics.Drawing
import Graphics.GLUtils
import Graphics.DrawText
import Math.Vector
import Types.Color

data Button a = Button (RelativeRect Scalar a) (Maybe Event, Maybe (ScreenAction a))

data MenuDrawCommand a = SquareMenuDrawCommand (RelativePos Float a, RelativePos Float a) Color (Maybe TexID) Shader
                     | TextMenuDrawCommand PrinterID TextCommand

data MenuResources = MenuResources [TexID] [PrinterID] [Shader]

type MenuResourceNames = ([String], [String], [(String, String)])

type UIRender a = [(RelativePos Scalar a, RelativePos Scalar a, MenuDrawCommand a)]

data MenuRenderSpec a = MenuRenderSpec MenuResourceNames (MenuResources -> Double -> UIRender a)

data UIElement a = UIElement (Maybe (Button a)) (MenuRenderSpec a)

data ResolvedUIElement a = ResolvedUIElement (Maybe (Button a)) (Double -> UIRender a)

data MenuScreen a = MenuScreen [UIElement a] Scalar

data ResolvedMenuScreen a = ResolvedMenuScreen [ResolvedUIElement a] Scalar

data ScreenAction a = PushScreen (MenuScreen a)
                  | RefreshScreen
                  | PopScreen

makeLabel :: MenuRenderSpec a -> UIElement a
makeLabel s = UIElement Nothing s
