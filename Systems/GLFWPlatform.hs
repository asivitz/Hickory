{-# LANGUAGE NamedFieldPuns #-}

module Systems.GLFWPlatform where

import Data.IORef
import qualified Data.HashSet as HashSet
import qualified Graphics.UI.GLFW as GLFW
import Types.Types
import Math.Vector
import Engine.Scene.Input

data SysData = SysData { 
             evlist :: [RawInput],
             touches :: HashSet.HashSet Int,
             fbSize :: Size Int
            }

empty = SysData { evlist = [],
                touches = HashSet.empty,
                fbSize = nullSize
                }


setupInput win addInput = do
        (width, height) <- GLFW.getFramebufferSize win
        sd <- newIORef (SysData [] HashSet.empty (Size width height))
        GLFW.setMouseButtonCallback win $ Just (mouseButtonCallback sd addInput)
        GLFW.setKeyCallback win $ Just (keyCallback sd addInput)
        return $ stepInput sd win addInput

stepInput sd win addInput = do
        SysData { fbSize } <- readIORef sd
        GLFW.pollEvents
        curPos <- GLFW.getCursorPos win
        let curloc = touchPosToScreenPos fbSize curPos
        addInput (InputTouchLoc curloc 0)

mouseButtonCallback :: IORef SysData -> (RawInput -> IO ()) -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> t -> IO ()
mouseButtonCallback platform addInput win button buttonState modkeys =
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

            writeIORef platform sd { touches = touches' }
            addInput (ev pos touchid)

keyCallback :: IORef SysData -> (RawInput -> IO ()) -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback platform addInput win key scancode keyState modkeys =
        do
            sd@SysData { evlist } <- readIORef platform

            curPos <- GLFW.getCursorPos win

            let ev = case keyState of
                        GLFW.KeyState'Pressed -> InputKeyDown
                        GLFW.KeyState'Released -> InputKeyUp

            addInput (ev key)

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

{-
broadcastTouchLoc win screensize touchid = do
        curPos <- liftIO $ GLFW.getCursorPos win
        let pos = touchPosToScreenPos screensize curPos
        runInterruptableEvent systemContext (\x -> x pos touchid) inputTouchLoc
        -}
