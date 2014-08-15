{-# LANGUAGE NamedFieldPuns #-}

module Systems.Input (makeIO) where

import Engine.System
import Engine.World
import Types.Types
import Utils.Utils
import Math.Vector

import qualified Graphics.UI.GLFW as GLFW
import qualified Systems.Draw as Draw
import Data.IORef
import Data.HashSet

import Control.Monad.State

makeIO draw = do
      sd <- newIORef $ SysData [] empty
      return $ System (run draw sd) (initS draw sd)

data SysData = SysData { 
             evlist :: [InputEv],
             touches :: HashSet Int
             }

broadcastTouchLoc win screenSize touchid = do
        curPos <- liftIO $ GLFW.getCursorPos win
        let pos = touchPosToScreenPos screenSize curPos
        runInterruptableEvent (\x -> x pos touchid) inputTouchLoc

processInputEv (InputTouchDown pos touchid) = do
        runInterruptableEvent (\x -> x pos touchid) inputTouchDown
processInputEv (InputTouchUp pos touchid) = do
        runInterruptableEvent (\x -> x pos touchid) inputTouchUp

run draw input delta = do
      Draw.SysData { Draw.window, Draw.screenSize } <- getSysData draw
      case window of
          Nothing -> return ()
          Just win -> do
              sd@SysData { evlist, touches } <- getSysData input
              {-mapM_ broadcast evlist-}
              mapM_ (broadcastTouchLoc win screenSize) $ toList touches

              mapM_ processInputEv evlist

              putSysData input (sd {evlist = []})

initS draw input = do
        Draw.SysData { Draw.window, Draw.screenSize } <- getSysData draw
        whenMaybe window $ \win ->
            liftIO $ GLFW.setMouseButtonCallback win $ Just (mouseButtonCallback input screenSize)
        return ()

touchIdent :: GLFW.MouseButton -> Int
touchIdent button = case button of
                       GLFW.MouseButton'1 -> 1
                       GLFW.MouseButton'2 -> 2
                       GLFW.MouseButton'3 -> 3
                       GLFW.MouseButton'4 -> 4
                       GLFW.MouseButton'5 -> 5
                       GLFW.MouseButton'6 -> 6
                       GLFW.MouseButton'7 -> 7
                       GLFW.MouseButton'8 -> 8

touchPosToScreenPos :: Size Int -> (Double, Double) -> V2
touchPosToScreenPos (Size w h) (x,y) = v2 (realToFrac x) ((fromIntegral h) - (realToFrac y))

data InputEv = InputTouchDown V2 Int
             | InputTouchUp V2 Int

mouseButtonCallback :: IORef SysData -> Size Int -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> t -> IO ()
mouseButtonCallback input screenSize win button buttonState modkeys =
        do
            sd@SysData { evlist, touches } <- readIORef input

            curPos <- GLFW.getCursorPos win

            let pos = touchPosToScreenPos screenSize curPos
                ev = case buttonState of
                        GLFW.MouseButtonState'Pressed -> InputTouchDown
                        GLFW.MouseButtonState'Released -> InputTouchUp
                touchid = touchIdent button
                touches' =
                    case buttonState of
                    GLFW.MouseButtonState'Pressed -> do
                        insert touchid touches
                    GLFW.MouseButtonState'Released -> do
                        delete touchid touches

            writeIORef input sd { touches = touches', evlist = (ev pos touchid) : evlist }
