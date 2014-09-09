{-# LANGUAGE NamedFieldPuns #-}

module Systems.GLFWPlatform where

import Engine.System
import Engine.World
import Data.IORef
import qualified Data.HashSet as HashSet
import qualified Graphics.UI.GLFW as GLFW
import Types.Types
import Utils.Utils
import Math.Vector
import Control.Monad.State
import Graphics.GLFWUtils
import Data.Traversable
import Engine.Input

data SysData = SysData { 
             evlist :: [InputEv],
             touches :: HashSet.HashSet Int,
             fbSize :: Size Int
            }

empty = SysData { evlist = [],
                touches = HashSet.empty,
                fbSize = nullSize
                }


makeGrabInput win = do
        (width, height) <- GLFW.getFramebufferSize win
        sd <- newIORef (SysData [] HashSet.empty (Size width height))
        GLFW.setMouseButtonCallback win $ Just (mouseButtonCallback sd)
        GLFW.setKeyCallback win $ Just (keyCallback sd)
        return $ grabInput sd win

grabInput :: IORef SysData -> GLFW.Window -> IO Input
grabInput sd win = do
        GLFW.pollEvents
        SysData { evlist, touches, fbSize } <- readIORef sd
        writeIORef sd (SysData [] HashSet.empty fbSize)
        curPos <- GLFW.getCursorPos win
        let curloc = touchPosToScreenPos fbSize curPos
        let evlist' = (InputTouchLoc curloc 0) : evlist
        return $ Input evlist'

mouseButtonCallback :: IORef SysData -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> t -> IO ()
mouseButtonCallback platform win button buttonState modkeys =
        do
            sd@SysData { fbSize, evlist, touches } <- readIORef platform

            curPos <- GLFW.getCursorPos win

            let pos = touchPosToScreenPos fbSize curPos
                ev = case buttonState of
                        GLFW.MouseButtonState'Pressed -> InputTouchDown
                        GLFW.MouseButtonState'Released -> InputTouchUp
                touchid = touchIdent button
                touches' =
                    case buttonState of
                    GLFW.MouseButtonState'Pressed -> do
                        HashSet.insert touchid touches
                    GLFW.MouseButtonState'Released -> do
                        HashSet.delete touchid touches

            writeIORef platform sd { touches = touches', evlist = (ev pos touchid) : evlist }

keyCallback :: IORef SysData -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback platform win key scancode keyState modkeys =
        do
            sd@SysData { evlist } <- readIORef platform

            curPos <- GLFW.getCursorPos win

            let ev = case keyState of
                        GLFW.KeyState'Pressed -> InputKeyDown
                        GLFW.KeyState'Released -> InputKeyUp

            writeIORef platform sd { evlist = (ev key) : evlist }

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

broadcastTouchLoc win screensize touchid = do
        curPos <- liftIO $ GLFW.getCursorPos win
        let pos = touchPosToScreenPos screensize curPos
        runInterruptableEvent systemContext (\x -> x pos touchid) inputTouchLoc

processInputEv (InputTouchDown pos touchid) = do
        runInterruptableEvent sysCon (\x -> x pos touchid) inputTouchDown
processInputEv (InputTouchUp pos touchid) = do
        runInterruptableEvent sysCon (\x -> x pos touchid) inputTouchUp
processInputEv (InputKeyUp key) = do
        runInterruptableEvent sysCon (\x -> x key) inputKeyUp
processInputEv (InputKeyDown key) = do
        runInterruptableEvent sysCon (\x -> x key) inputKeyDown
