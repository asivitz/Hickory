module Menus.Menus where

import Types.Types
import Graphics.Drawing
import Graphics.GLUtils
import Graphics.DrawText
import Math.Vector
import Types.Color

data Button a c = Button (RelativeRect Scalar a) ([c], Maybe (ScreenAction a c))

data MenuDrawCommand a = SquareMenuDrawCommand (RelativePos Float a, RelativePos Float a) Color (Maybe TexID) Shader
                     | TextMenuDrawCommand PrinterID TextCommand

data MenuResources = MenuResources [TexID] [PrinterID] [Shader]

type MenuResourceNames = ([String], [String], [(String, String)])

type UIRender a = [(RelativePos Scalar a, RelativePos Scalar a, MenuDrawCommand a)]

data MenuRenderSpec a = MenuRenderSpec MenuResourceNames (MenuResources -> Double -> Bool -> UIRender a)

data UIElement a c = UIElement (Maybe (Button a c)) (MenuRenderSpec a)

data ResolvedUIElement a c = ResolvedUIElement (Maybe (Button a c)) (Double -> Bool -> UIRender a)

data MenuScreen a c = MenuScreen [UIElement a c] Scalar

data ResolvedMenuScreen a c = ResolvedMenuScreen [ResolvedUIElement a c] Scalar

data ScreenAction a c = PushScreen (MenuScreen a c)
                  | RefreshScreen
                  | PopScreen

makeLabel :: MenuRenderSpec a -> UIElement a c
makeLabel s = UIElement Nothing s
