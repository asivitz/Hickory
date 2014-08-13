{-# LANGUAGE NamedFieldPuns #-}

module Systems.Menus where

import Menus.Menus

import Math.Vector
import Types.Types
import Engine.World
import Engine.System

import Utils.Utils

import Graphics.Drawing
import Graphics.DrawText

import Control.Monad.State
import Data.Maybe
import Data.IORef

import qualified Systems.Draw as Draw
import qualified Systems.DrawText as DrawText
import qualified Systems.Textures as Textures

data SysData c = SysData [ResolvedMenuScreen Scalar c] Double (Maybe (ResolvedMenuScreen Scalar c))

empty = SysData [] 0 Nothing

make menus draw texes dt = System (run menus draw dt) (initS menus draw texes dt)


handleAction menus draw texes dt (PushScreen scr) = pushScreen menus draw texes dt scr 
handleAction menus draw texes dt RefreshScreen = return ()
handleAction menus draw texes dt PopScreen = popScreen menus

initS menus draw texes dt = do
        registerEvent inputTouchUp (inputTouchUp' menus draw texes dt)

inputTouchUp' menus draw texes dt pos touchid = do
        SysData navstack time leaving <- getSysData menus
        whenMaybe (listToMaybe navstack) $ \(ResolvedMenuScreen elements duration) -> 
            when (time > duration) $
                handleScreenClick menus draw texes dt pos elements

handleScreenClick menus draw texes dt pos elements = do
        Draw.SysData { Draw.screenSize } <- getSysData draw
        mapM_ (\(ResolvedUIElement but _) -> 
                case but of
                    Just (Button rrect (mevent, maction)) -> do
                        when (posInRect pos (transformRect rrect screenSize)) $ do
                            sequence_ mevent
                            whenMaybe maction $ \a -> handleAction menus draw texes dt a
                    Nothing -> return ()
            )
            elements

incomingScreen :: SysData c -> Maybe (ResolvedMenuScreen Scalar c)
incomingScreen (SysData [] _ _) = Nothing
incomingScreen (SysData (x:_) _ _) = Just x

leavingScreen :: SysData c -> Maybe (ResolvedMenuScreen Scalar c)
leavingScreen (SysData _ _ (Just x)) = Just x
leavingScreen (SysData (x:y:_) _ Nothing) = Just y
leavingScreen (SysData _ _ _) = Nothing

menuRender :: Real a => IORef DrawText.SysData -> Size a -> V3 -> MenuDrawCommand Scalar -> SysMonad c IO ()
menuRender dt (Size w h) pos (SquareMenuDrawCommand (rpw, rph) color mtex sh) = liftIO $ Draw.drawSpec pos uiLabel spec
    where size = Size (transform rpw w) (transform rph h)
          spec = case mtex of
                     Nothing -> SolidSquare size color sh
                     Just tex -> Square size color tex sh

menuRender dt screenSize pos (TextMenuDrawCommand pid tc) = DrawText.drawText dt pid uiLabel (PositionedTextCommand pos tc)

drawElements :: Real a => IORef DrawText.SysData -> Size a -> [ResolvedUIElement Scalar c] -> Double -> Bool -> SysMonad c IO ()
drawElements dt screenSize elements fraction incoming = mapM_ (\(ResolvedUIElement _ renderfunc) -> 
    mapM_ (\(yloc, xloc, mdc) -> menuRender dt screenSize (screenPos screenSize yloc xloc) mdc) (renderfunc fraction incoming))
        elements

drawMenus :: Real a => SysData c -> IORef DrawText.SysData -> Size a -> SysMonad c IO ()
drawMenus sd@(SysData navstack time leaving) dt screenSize = do
        whenMaybe (incomingScreen sd) $ \(ResolvedMenuScreen incEles duration) -> do
            let fraction = time / duration
                pushing = isNothing leaving
            when (fraction < 1) $ do
                let leavScreen = leavingScreen sd
                whenMaybe leavScreen $ \(ResolvedMenuScreen eles _) -> drawElements dt screenSize eles (1 - fraction) pushing

            drawElements dt screenSize incEles (min 1 fraction) (not pushing)

run menus draw dt delta = do
        SysData navstack time leaving <- getSysData menus
        let sd' = SysData navstack (time + delta) leaving
        putSysData menus sd'

        Draw.SysData { Draw.screenSize } <- getSysData draw
        drawMenus sd' dt screenSize



resolveUIElement :: IORef Draw.SysData -> IORef Textures.SysData -> IORef DrawText.SysData -> UIElement Scalar c -> SysMonad c IO (Maybe (ResolvedUIElement Scalar c))
resolveUIElement draw texes dt (UIElement but (MenuRenderSpec (tidnames, printernames, shadernames) func)) = do
        RPC { _reserveTex, _reservePrinter } <- getRPC
        tids <- mapM _reserveTex tidnames
        pids <- mapM _reservePrinter printernames
        shaders <- mapM (Draw.reserveShader draw) shadernames
        
        if any isNothing tids ||
            any isNothing pids ||
            any isNothing shaders
            then return Nothing
            else
                return $ Just $ ResolvedUIElement but (func (MenuResources (catMaybes tids) (catMaybes pids) (catMaybes shaders)))

resolveScreen :: IORef Draw.SysData -> IORef Textures.SysData -> IORef DrawText.SysData -> (MenuScreen Scalar c) -> SysMonad c IO (ResolvedMenuScreen Scalar c)
resolveScreen draw texes dt (MenuScreen eles dur) = do 
                                                       res_eles <- mapM (resolveUIElement draw texes dt) eles
                                                       return $ ResolvedMenuScreen (catMaybes res_eles) dur

pushScreen menus draw texes dt screen = do
        SysData navstack time leaving <- getSysData menus
        screen' <- (resolveScreen draw texes dt) screen
        putSysData menus (SysData (screen':navstack) 0 Nothing)

popScreen menus = do
        SysData navstack time _ <- getSysData menus
        case navstack of
            (x:xs) -> putSysData menus (SysData xs 0 (Just x))
            _ -> liftIO $ print "Can't pop menu navstack." >> return ()
