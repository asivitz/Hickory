{-# LANGUAGE NamedFieldPuns #-}

module Systems.Menus where

import Menus.Menus

import Math.Vector
import Types.Types
import Engine.Event
import Engine.System

import Utils.Utils

import Graphics.Drawing
import Graphics.DrawText
import Graphics.GLUtils

import Control.Monad.State
import Data.Maybe
import Data.IORef

import qualified Systems.Draw as Draw
import qualified Systems.DrawText as DrawText
import qualified Systems.Textures as Textures

data SysData = SysData [ResolvedMenuScreen]

empty = SysData []

make menus draw texes dt = System (run menus draw dt) (handleEv menus draw texes dt) nullInit

menuRender dt (Size w h) pos (SquareMenuDrawCommand (rpw, rph) color mtex sh) = liftIO $ Draw.drawSpec pos uiLabel spec
    where size = Size (transform rpw w) (transform rph h)
          spec = case mtex of
                     Nothing -> SolidSquare size color sh
                     Just tex -> Square size color tex sh

menuRender dt screenSize pos (TextMenuDrawCommand pid tc) = DrawText.drawText dt pid uiLabel (PositionedTextCommand pos tc)

handleAction menus draw texes dt (PushScreen scr) = pushScreen menus draw texes dt scr
handleAction menus draw texes dt RefreshScreen = return ()
handleAction menus draw texes dt PopScreen = popScreen menus

handleEv menus draw texes dt event =
        case event of
            (InputTouchUp pos touchid) -> do
                SysData navstack <- getSysData menus
                mapM_ (\(ResolvedMenuScreen elements duration) -> 
                    mapM_ (\(ResolvedUIElement but _) -> 
                            case but of
                                Just (Button rect (mevent, maction)) -> do
                                    when (posInRect pos rect) $ do
                                        whenMaybe mevent $ \e -> broadcast e
                                        whenMaybe maction $ \a -> handleAction menus draw texes dt a
                                Nothing -> return ()
                        )
                        elements
                    )
                    navstack
            _ -> return ()

run menus draw dt delta = do
        SysData navstack <- getSysData menus
        Draw.SysData { Draw.screenSize } <- getSysData draw
        mapM_ (\(ResolvedMenuScreen elements duration) -> 
            mapM_ (\(ResolvedUIElement _ uirender) -> 
                mapM_ (\(yloc, xloc, mdc) -> menuRender dt screenSize (screenPos screenSize yloc xloc) mdc) uirender)
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

popScreen menus = do
        SysData navstack <- getSysData menus
        case navstack of
            (x:xs) -> putSysData menus (SysData xs)
            _ -> return ()
