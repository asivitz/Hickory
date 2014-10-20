{-# LANGUAGE NamedFieldPuns #-}

module GLFW.Platform where

import Data.IORef
import qualified Data.HashMap.Strict as HashMap
import qualified Graphics.UI.GLFW as GLFW
import Types.Types
import Math.Vector
import Engine.Scene.Input
import Data.Time
import Data.Maybe

--TODO: RawInput Int should instead use a generic engine key type, and then
    --a method of converting GLFW.Key to it

data SysData = SysData { 
             touches :: HashMap.HashMap Int UTCTime,
             fbSize :: Size Int
            }

empty = SysData {
                touches = HashMap.empty,
                fbSize = nullSize
                }


setupInput win addInput = do
        (width, height) <- GLFW.getFramebufferSize win
        sd <- newIORef (SysData HashMap.empty (Size width height))
        GLFW.setMouseButtonCallback win $ Just (mouseButtonCallback sd addInput)
        GLFW.setKeyCallback win $ Just (keyCallback sd addInput)
        return $ stepInput sd win addInput

stepInput sd win addInput = do
        SysData { fbSize, touches } <- readIORef sd
        GLFW.pollEvents
        curPos <- GLFW.getCursorPos win
        let curloc = touchPosToScreenPos fbSize curPos
            ident = case listToMaybe $ HashMap.keys touches of
                        Nothing -> 0
                        Just a -> a
        addInput (InputTouchLoc curloc ident)

mouseButtonCallback :: IORef SysData -> (RawInput Int -> IO ()) -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> t -> IO ()
mouseButtonCallback platform addInput win button buttonState modkeys =
        do
            sd@SysData { fbSize, touches } <- readIORef platform

            curPos <- GLFW.getCursorPos win

            let pos = touchPosToScreenPos fbSize curPos
                touchid = touchIdent button
            (ev, touches') <-
                    case buttonState of
                        GLFW.MouseButtonState'Pressed -> do
                            time <- getCurrentTime
                            return (InputTouchDown, HashMap.insert touchid time touches)
                        GLFW.MouseButtonState'Released ->
                            case HashMap.lookup touchid touches of
                                Nothing -> return (InputTouchUp 0, touches)
                                Just prev -> do
                                    time <- getCurrentTime
                                    let delta = realToFrac (diffUTCTime time prev)
                                    return (InputTouchUp delta, HashMap.delete touchid touches)


            writeIORef platform sd { touches = touches' }
            addInput (ev pos touchid)

keyCallback :: IORef SysData -> (RawInput Int -> IO ()) -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback platform addInput win key scancode keyState modkeys =
        do
            curPos <- GLFW.getCursorPos win

            let ev = case keyState of
                        GLFW.KeyState'Pressed -> InputKeyDown
                        GLFW.KeyState'Released -> InputKeyUp

            addInput (ev 9342)

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
