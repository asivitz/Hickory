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

make :: SysMonad c IO (System c)
make = do
        win <- liftIO $ buildWindow 400 400 "Hi hi!"

        (fbWidth, fbHeight) <- case win of
            Nothing -> return (0,0)
            Just w -> liftIO $ GLFW.getFramebufferSize w

        sd <- liftIO $ newIORef empty { window = win, fbSize = (Size fbWidth fbHeight) }

        whenMaybe win $ \window ->
            liftIO $ GLFW.setMouseButtonCallback window $ Just (mouseButtonCallback sd)

        registerResource running (running' sd)
        registerResource screenSize (screenSize' sd)

        return $ System (run sd)

run platform delta = do
        liftIO GLFW.pollEvents

        sd@SysData { window, fbSize, evlist, touches } <- getSysData platform

        liftIO $ traverse GLFW.swapBuffers window

        whenMaybe window $ \win -> do
            mapM_ (broadcastTouchLoc win fbSize) $ HashSet.toList touches
            mapM_ processInputEv evlist

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
        putSysData platform sd { isRunning = isRunning', evlist = [] }

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
             | InputTouchUp V2 Int deriving (Show)

broadcastTouchLoc win screensize touchid = do
        curPos <- liftIO $ GLFW.getCursorPos win
        let pos = touchPosToScreenPos screensize curPos
        runInterruptableEvent (\x -> x pos touchid) inputTouchLoc

processInputEv (InputTouchDown pos touchid) = do
        runInterruptableEvent (\x -> x pos touchid) inputTouchDown
processInputEv (InputTouchUp pos touchid) = do
        runInterruptableEvent (\x -> x pos touchid) inputTouchUp
