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

data SysData = SysData { 
             window :: Maybe (GLFW.Window),
             fbSize :: Size Int,
             isRunning :: Bool,
             evlist :: [InputEv],
             touches :: HashSet.HashSet Int
            }

empty = SysData { window = Nothing,
                fbSize = nullSize, 
                isRunning = True,
                evlist = [],
                touches = HashSet.empty
                }

running' platform = do
        SysData { isRunning } <- readIORef platform
        return isRunning 

screenSize' platform = do
        SysData { fbSize } <- getSysData platform
        return fbSize

runPre platform delta = do
        liftIO GLFW.pollEvents

        sd@SysData { window, fbSize, evlist, touches } <- getSysData platform

        whenMaybe window $ \win -> do
            mapM_ (broadcastTouchLoc win fbSize) $ HashSet.toList touches
            mapM_ processInputEv evlist

        putSysData platform sd { evlist = [] }

make :: SysMonad c IO (System c, System c)
make = do
        win <- liftIO $ buildWindow 800 800 "Hi hi!"

        (fbWidth, fbHeight) <- case win of
            Nothing -> return (0,0)
            Just w -> liftIO $ GLFW.getFramebufferSize w

        sd <- liftIO $ newIORef empty { window = win, fbSize = (Size fbWidth fbHeight) }

        whenMaybe win $ \window -> do
            liftIO $ GLFW.setMouseButtonCallback window $ Just (mouseButtonCallback sd)
            liftIO $ GLFW.setKeyCallback window $ Just (keyCallback sd)
            registerResource sysCon inputKeyGetState (\k -> liftIO $ GLFW.getKey window k)


        registerResource sysCon running (running' sd)
        registerResource sysCon screenSize (screenSize' sd)
        registerEvent sysCon inputKeyUp $ \k -> do
            when (k == GLFW.Key'D) $ runEventId sysCon printAll
            return False

        return (System (runPre sd), System (run sd))

run platform delta = do
        sd@SysData { window } <- getSysData platform

        liftIO $ traverse GLFW.swapBuffers window

        isRunning' <- liftIO $ do
            q <- traverse GLFW.windowShouldClose window
            case q of
                Nothing -> return True
                Just False -> return True
                Just True -> do
                    traverse GLFW.destroyWindow window
                    GLFW.terminate
                    return False

        -- Just remove processed events!
        putSysData platform sd { isRunning = isRunning' }

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
