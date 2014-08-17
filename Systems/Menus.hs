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

type MenuEvent c m = SysMonad c m ()

data SysData c = SysData [ResolvedMenuScreen Scalar c] Double (Maybe (ResolvedMenuScreen Scalar c))

empty = SysData [] 0 Nothing

make :: SysMonad c IO (System c)
make = do
        menus <- liftIO $ newIORef empty
        initS menus
        return $ System (run menus)


handleAction menus (PushScreen scr) = pushScreen menus scr 
handleAction menus RefreshScreen = return ()
handleAction menus PopScreen = popScreen menus

initS menus = do
        registerEvent inputTouchUp (inputTouchUp' menus)
        registerResource pushMenuScreen (pushScreen menus)

inputTouchUp' menus pos touchid = do
        SysData navstack time leaving <- getSysData menus
        case (listToMaybe navstack) of
            Just (ResolvedMenuScreen elements duration) -> 
                if (time > duration) then
                                     handleScreenClick menus pos elements
                                     else return False
            Nothing -> return False

handleScreenClick menus pos elements = do
        RPC { _screenSize } <- getRPC
        sSize <- _screenSize

        res <- mapM (\(ResolvedUIElement but _) -> 
                case but of
                    Just (Button rrect (mevent, maction)) -> do
                        if (posInRect pos (transformRect rrect sSize)) 
                            then do
                                sequence_ mevent
                                whenMaybe maction $ \a -> handleAction menus a
                                return True
                            else
                                return False
                    Nothing -> return False
            )
            elements
        return $ or res

incomingScreen :: SysData c -> Maybe (ResolvedMenuScreen Scalar c)
incomingScreen (SysData [] _ _) = Nothing
incomingScreen (SysData (x:_) _ _) = Just x

leavingScreen :: SysData c -> Maybe (ResolvedMenuScreen Scalar c)
leavingScreen (SysData _ _ (Just x)) = Just x
leavingScreen (SysData (x:y:_) _ Nothing) = Just y
leavingScreen (SysData _ _ _) = Nothing

menuRender :: Real a => Size a -> V3 -> MenuDrawCommand Scalar -> SysMonad c IO ()
menuRender (Size w h) pos (SquareMenuDrawCommand (rpw, rph) color mtex sh) = liftIO $ Draw.drawSpec pos uiLabel spec
    where size = Size (transform rpw w) (transform rph h)
          spec = case mtex of
                     Nothing -> SolidSquare size color sh
                     Just tex -> Square size color tex sh

menuRender screensize pos (TextMenuDrawCommand pid tc) = do
        RPC { _drawText } <- getRPC
        _drawText pid uiLabel (PositionedTextCommand pos tc)

drawElements :: (Real a, Monad m) => Size a -> [ResolvedUIElement Scalar (MenuEvent c m)] -> Double -> Bool -> SysMonad c IO ()
drawElements screensize elements fraction incoming = mapM_ (\(ResolvedUIElement _ renderfunc) -> 
    mapM_ (\(yloc, xloc, mdc) -> menuRender screensize (screenPos screensize yloc xloc) mdc) (renderfunc fraction incoming))
        elements

drawMenus :: (Real a, Monad m) => SysData (MenuEvent c m) -> Size a -> SysMonad c IO ()
drawMenus sd@(SysData navstack time leaving) screensize = do
        whenMaybe (incomingScreen sd) $ \(ResolvedMenuScreen incEles duration) -> do
            let fraction = time / duration
                pushing = isNothing leaving
            when (fraction < 1) $ do
                let leavScreen = leavingScreen sd
                whenMaybe leavScreen $ \(ResolvedMenuScreen eles _) -> drawElements screensize eles (1 - fraction) pushing

            drawElements screensize incEles (min 1 fraction) (not pushing)

run menus delta = do
        SysData navstack time leaving <- getSysData menus
        let sd' = SysData navstack (time + delta) leaving
        putSysData menus sd'

        RPC { _screenSize } <- getRPC
        ss <- _screenSize
        drawMenus sd' ss



resolveUIElement :: Monad m => UIElement Scalar (MenuEvent c m) -> SysMonad c IO (Maybe (ResolvedUIElement Scalar (MenuEvent c m)))
resolveUIElement (UIElement but (MenuRenderSpec (tidnames, printernames, shadernames) func)) = do
        RPC { _reserveTex, _reservePrinter, _reserveShader } <- getRPC
        tids <- mapM _reserveTex tidnames
        pids <- mapM _reservePrinter printernames
        shaders <- mapM _reserveShader shadernames
        
        if any isNothing tids ||
            any isNothing pids ||
            any isNothing shaders
            then return Nothing
            else
                return $ Just $ ResolvedUIElement but (func (MenuResources (catMaybes tids) (catMaybes pids) (catMaybes shaders)))

resolveScreen :: Monad m => (MenuScreen Scalar (MenuEvent c m)) -> SysMonad c IO (ResolvedMenuScreen Scalar (MenuEvent c m))
resolveScreen (MenuScreen eles dur) = do 
                                                       res_eles <- mapM resolveUIElement eles
                                                       return $ ResolvedMenuScreen (catMaybes res_eles) dur

pushScreen :: Monad m => IORef (SysData (MenuEvent c m)) -> MenuScreen Scalar (MenuEvent c m) -> SysMonad c IO ()
pushScreen menus screen = do
        SysData navstack time leaving <- getSysData menus
        screen' <- resolveScreen screen
        putSysData menus (SysData (screen':navstack) 0 Nothing)

popScreen menus = do
        SysData navstack time _ <- getSysData menus
        case navstack of
            (x:xs) -> putSysData menus (SysData xs 0 (Just x))
            _ -> liftIO $ print "Can't pop menu navstack." >> return ()
