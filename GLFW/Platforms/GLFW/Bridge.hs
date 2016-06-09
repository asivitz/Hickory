{-# LANGUAGE NamedFieldPuns #-}

module Platforms.GLFW.Bridge where

import Data.IORef
import qualified Data.HashMap.Strict as HashMap
import qualified Graphics.UI.GLFW as GLFW
import Types.Types
import Math.Vector
import Engine.Scene.Input
import Data.Time
import Data.Maybe
import Control.Monad

--TODO: RawInput Int should instead use a generic engine key type, and then
    --a method of converting GLFW.Key to it

data SysData = SysData { 
             touches :: HashMap.HashMap Int UTCTime,
             keys :: HashMap.HashMap Key UTCTime,
             fbSize :: Size Int
            }

empty = SysData {
                touches = HashMap.empty,
                keys = HashMap.empty,
                fbSize = nullSize
                }


setupInput win addInput = do
        (width, height) <- GLFW.getFramebufferSize win
        sd <- newIORef (SysData HashMap.empty HashMap.empty (Size width height))
        GLFW.setMouseButtonCallback win $ Just (mouseButtonCallback sd addInput)
        GLFW.setKeyCallback win $ Just (keyCallback sd addInput)
        GLFW.setFramebufferSizeCallback win $ Just (framebufferSizeCallback sd)
        return $ stepInput sd win addInput

stepInput sd win addInput = do
        SysData { fbSize, touches, keys } <- readIORef sd
        GLFW.pollEvents
        curPos <- GLFW.getCursorPos win
        let curloc = touchPosToScreenPos fbSize curPos
            ident = fromMaybe 0 $ listToMaybe $ HashMap.keys touches
        addInput (InputTouchesLoc [(curloc,ident)])

        unless (HashMap.null keys) $ do
                time <- getCurrentTime
                let relative = HashMap.map (\t -> realToFrac (diffUTCTime time t)) keys
                addInput (InputKeysHeld relative)

framebufferSizeCallback :: IORef SysData -> GLFW.Window -> Int -> Int -> IO ()
framebufferSizeCallback platform win width height = modifyIORef platform (\sd@SysData { fbSize } -> sd { fbSize = Size width height })

mouseButtonCallback :: IORef SysData -> (RawInput -> IO ()) -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> t -> IO ()
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
                            return (InputTouchesDown [(pos,touchid)], HashMap.insert touchid time touches)
                        GLFW.MouseButtonState'Released ->
                            case HashMap.lookup touchid touches of
                                Nothing -> return (InputTouchesUp [(0,pos,touchid)], touches)
                                Just prev -> do
                                    time <- getCurrentTime
                                    let delta = realToFrac (diffUTCTime time prev)
                                    return (InputTouchesUp [(delta,pos,touchid)], HashMap.delete touchid touches)

            writeIORef platform sd { touches = touches' }
            addInput ev

keyCallback :: IORef SysData -> (RawInput -> IO ()) -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback platform addInput win glfwkey scancode keyState modkeys =
        do
            sd@SysData { keys } <- readIORef platform

            time <- getCurrentTime
            let key = toEnum (fromEnum glfwkey) -- only possible since our Key type is defined exactly the same as GLFW's
                -- other platforms will need a more intelligent conversion
                -- between key values
            case keyState of
                GLFW.KeyState'Pressed -> do
                    addInput (InputKeyDown key)
                    writeIORef platform sd { keys = HashMap.insert key time keys }
                GLFW.KeyState'Released ->
                    case HashMap.lookup key keys of
                        Nothing -> addInput (InputKeyUp key 0)
                        Just prev -> do
                            addInput (InputKeyUp key (realToFrac (diffUTCTime time prev)))
                            writeIORef platform sd { keys = HashMap.delete key keys }
                _ -> return ()

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
touchPosToScreenPos (Size w h) (x,y) = v2 (realToFrac x) (fromIntegral h - realToFrac y)

{-
broadcastTouchLoc win screensize touchid = do
        curPos <- liftIO $ GLFW.getCursorPos win
        let pos = touchPosToScreenPos screensize curPos
        runInterruptableEvent systemContext (\x -> x pos touchid) inputTouchLoc
        -}
