{-# LANGUAGE NamedFieldPuns #-}

module Systems.Menus where

import Math.Vector
import Types.Types
import Engine.Event
import Engine.System

import Graphics.Drawing
import Graphics.DrawText
import Graphics.GLUtils

import Control.Monad.State
import Data.Maybe
import Data.IORef

import qualified Systems.Draw as Draw
import qualified Systems.DrawText as DrawText
import qualified Systems.Textures as Textures

data Button = Button Rect (Maybe Event, Maybe ScreenAction)

data MenuDrawCommand = SquareMenuDrawCommand DrawSpec
                     | TextMenuDrawCommand DrawText.PrinterID TextCommand

data MenuResources = MenuResources [TexID] [DrawText.PrinterID] [Shader]

type MenuResourceNames = ([String], [String], [(String, String)])

type UIRender a = [(YScreenLoc a, XScreenLoc a, MenuDrawCommand)]

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

posInRect (Vector2 px py) (Rect (Vector2 ox oy) (Size w h)) =
        ((abs (ox - px)) < (w/2)) && ((abs (oy - py)) < (h/2))

data SysData = SysData [ResolvedMenuScreen]

empty = SysData []

make menus draw dt = System (run menus draw dt) nullHandleEvent nullInit

menuRender dt pos (SquareMenuDrawCommand spec) = liftIO $ Draw.drawSpec pos uiLabel spec
menuRender dt pos (TextMenuDrawCommand pid tc) = DrawText.drawText dt pid uiLabel (PositionedTextCommand pos tc)

run menus draw dt delta = do
        SysData navstack <- getSysData menus
        Draw.SysData { Draw.screenSize } <- getSysData draw
        mapM_ (\(ResolvedMenuScreen elements duration) -> 
            mapM_ (\(ResolvedUIElement _ uirender) -> 
                mapM_ (\(yloc, xloc, mdc) -> menuRender dt (screenPos screenSize yloc xloc) mdc) uirender)
                elements)
            navstack

resolveUIElement :: IORef Draw.SysData -> IORef Textures.SysData -> IORef DrawText.SysData -> UIElement -> SysMonad IO (Maybe ResolvedUIElement)
resolveUIElement draw texes dt (UIElement but (MenuRenderSpec (tidnames, printernames, shadernames) func)) = do
        tids <- mapM (Textures.reserveTex texes) tidnames
        pids <- mapM (DrawText.reservePrinter dt texes) printernames
        shaders <- mapM (Draw.reserveShader draw) shadernames
        
        if any isNothing tids ||
            any isNothing pids ||
            any isNothing shaders
            then return Nothing
            else
                return $ Just $ ResolvedUIElement but (func (MenuResources (catMaybes tids) (catMaybes pids) (catMaybes shaders)))

resolveScreen :: IORef Draw.SysData -> IORef Textures.SysData -> IORef DrawText.SysData -> MenuScreen -> SysMonad IO ResolvedMenuScreen
resolveScreen draw texes dt (MenuScreen eles dur) = do 
                                                       res_eles <- mapM (resolveUIElement draw texes dt) eles
                                                       return $ ResolvedMenuScreen (catMaybes res_eles) dur

pushScreen menus draw texes dt screen = do
        SysData navstack <- getSysData menus
        screen' <- (resolveScreen draw texes dt) screen
        putSysData menus (SysData (screen':navstack))

