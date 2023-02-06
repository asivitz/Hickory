{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Platforms.GLFW.Bridge where

import Data.IORef
import qualified Data.HashMap.Strict as HashMap
import qualified Graphics.UI.GLFW as GLFW
import Hickory.Types
import Hickory.Math.Vector
import Hickory.Input
import Data.Time
import Data.Maybe
import Linear (V2(..))
import Control.Monad.Extra (unlessM)
import DearImGui (wantCaptureMouse, wantCaptureKeyboard)
import Data.HashMap.Strict (HashMap)
import Data.Traversable (for)

--TODO: RawInput Int should instead use a generic engine key type, and then
    --a method of converting GLFW.Key to it

data InputData = InputData
  { touches  :: IORef (HashMap Int UTCTime)
  , keys     :: IORef (HashMap Key UTCTime)
  , gamepads :: IORef (HashMap Int GLFW.GamepadState)
  }

-- Userspace likely cares about the window size, not framebuffer size.
-- (Could be different for high DPI displays)
getWindowSizeRef :: GLFW.Window -> IO (IORef (Size Int))
getWindowSizeRef win = do
  wSize     <- uncurry Size <$> GLFW.getWindowSize win
  wSizeRef  <- newIORef wSize
  GLFW.setWindowSizeCallback win . Just $ \_ w h -> do
    writeIORef wSizeRef (Size w h)

  pure wSizeRef

getCurrentGamePadIds :: IO [Int]
getCurrentGamePadIds = HashMap.keys <$> getCurrentGamePads

getCurrentGamePads :: IO (HashMap Int GLFW.GamepadState)
getCurrentGamePads = HashMap.fromList . catMaybes <$> for [minBound..maxBound] \js ->
  fmap (fromEnum js,) <$> GLFW.getGamepadState js

setupInput :: GLFW.Window -> (RawInput -> IO ()) -> IO (IO ())
setupInput win addInput = do
  initialGamepads <- getCurrentGamePads

  indat <- InputData
    <$> newIORef mempty
    <*> newIORef mempty
    <*> newIORef initialGamepads
  GLFW.setMouseButtonCallback win $ Just (mouseButtonCallback indat addInput)
  GLFW.setKeyCallback win $ Just (keyCallback indat addInput)
  GLFW.setJoystickCallback $ Just (joystickCallback indat addInput)

  pure $ stepInput indat win addInput

stepInput :: InputData -> GLFW.Window -> (RawInput -> IO ()) -> IO ()
stepInput indat win addInput = do
  touches <- readIORef indat.touches
  GLFW.pollEvents
  curPos <- GLFW.getCursorPos win
  let curloc = touchPosToScreenPos curPos
      ident = fromMaybe 0 $ listToMaybe $ HashMap.keys touches
  addInput (InputTouchesLoc [(curloc,ident)])

  newGamePads <- (HashMap.toList <$> readIORef indat.gamepads) >>= traverse \(toEnum -> js, _gs) -> do
    GLFW.getGamepadState js >>= \case
      Just gsNew -> do
        let GLFW.GamepadState getButtonState getAxisState = gsNew
            gp = GamePad {..}
            leftStick = V2 (getAxisState GLFW.GamepadAxis'LeftX)
                           (getAxisState GLFW.GamepadAxis'LeftY)
            rightStick = V2 (getAxisState GLFW.GamepadAxis'RightX)
                            (getAxisState GLFW.GamepadAxis'RightY)
            leftTrigger  = getAxisState GLFW.GamepadAxis'LeftTrigger
            rightTrigger = getAxisState GLFW.GamepadAxis'RightTrigger

            toButton = \case
              GLFW.GamepadButtonState'Pressed  -> Pressed
              GLFW.GamepadButtonState'Released -> Released
            a = toButton $ getButtonState GLFW.GamepadButton'A
            b = toButton $ getButtonState GLFW.GamepadButton'B
            x = toButton $ getButtonState GLFW.GamepadButton'X
            y = toButton $ getButtonState GLFW.GamepadButton'Y
            leftBumper = toButton $ getButtonState GLFW.GamepadButton'LeftBumper
            rightBumper = toButton $ getButtonState GLFW.GamepadButton'RightBumper
            back = toButton $ getButtonState GLFW.GamepadButton'Back
            start = toButton $ getButtonState GLFW.GamepadButton'Start
            guide = toButton $ getButtonState GLFW.GamepadButton'Guide
            leftThumb = toButton $ getButtonState GLFW.GamepadButton'LeftThumb
            rightThumb = toButton $ getButtonState GLFW.GamepadButton'RightThumb
            dpadUp = toButton $ getButtonState GLFW.GamepadButton'DpadUp
            dpadRight = toButton $ getButtonState GLFW.GamepadButton'DpadRight
            dpadDown = toButton $ getButtonState GLFW.GamepadButton'DpadDown
            dpadLeft = toButton $ getButtonState GLFW.GamepadButton'DpadLeft
            cross = toButton $ getButtonState GLFW.GamepadButton'Cross
            circle = toButton $ getButtonState GLFW.GamepadButton'Circle
            square = toButton $ getButtonState GLFW.GamepadButton'Square
            triangle = toButton $ getButtonState GLFW.GamepadButton'Triangle

        addInput $ InputGamePad (fromEnum js) gp

        pure $ Just (fromEnum js, gsNew)

      Nothing -> pure Nothing
  writeIORef indat.gamepads (HashMap.fromList $ catMaybes newGamePads)

  time <- getCurrentTime

  ks <- readIORef indat.keys
  let relative = HashMap.map (realToFrac . diffUTCTime time) ks
  addInput (InputKeysHeld relative)

mouseButtonCallback :: InputData -> (RawInput -> IO ()) -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> t -> IO ()
mouseButtonCallback indat addInput win button buttonState _modkeys = unlessM wantCaptureMouse do
  touches <- readIORef indat.touches

  curPos <- GLFW.getCursorPos win

  let pos = touchPosToScreenPos curPos
      touchid = glfwTouchIdent button

  (ev, touches') <- case buttonState of
    GLFW.MouseButtonState'Pressed -> do
      time <- getCurrentTime
      return (InputTouchesDown [(pos,touchid)], HashMap.insert touchid time touches)
    GLFW.MouseButtonState'Released -> case HashMap.lookup touchid touches of
      Nothing -> return (InputTouchesUp [(0,pos,touchid)], touches)
      Just prev -> do
        time <- getCurrentTime
        let delta = realToFrac (diffUTCTime time prev)
        return (InputTouchesUp [(delta,pos,touchid)], HashMap.delete touchid touches)

  writeIORef indat.touches touches'
  addInput ev

keyCallback :: InputData -> (RawInput -> IO ()) -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback indat addInput _win glfwkey _scancode keyState _modkeys = unlessM wantCaptureKeyboard do
  keys <- readIORef indat.keys

  time <- getCurrentTime
  let key = toEnum (fromEnum glfwkey) -- only possible since our Key type is defined exactly the same as GLFW's
      -- other platforms will need a more intelligent conversion
      -- between key values
  case keyState of
    GLFW.KeyState'Pressed -> do
      addInput (InputKeyDown key)
      modifyIORef' indat.keys $ HashMap.insert key time
    GLFW.KeyState'Released ->
      case HashMap.lookup key keys of
        Nothing -> addInput (InputKeyUp key 0)
        Just prev -> do
          addInput (InputKeyUp key (realToFrac (diffUTCTime time prev)))
          modifyIORef' indat.keys $ HashMap.delete key
    _ -> return ()

joystickCallback :: InputData -> (RawInput -> IO ()) -> GLFW.Joystick ->  GLFW.JoystickState -> IO ()
joystickCallback indat addInput js jsst = do
  GLFW.getGamepadState js >>= \case
    Just gs -> do
      case jsst of
        GLFW.JoystickState'Connected    -> addInput $ InputGamePadConnection (fromEnum js) True
        _ -> pure ()

      modifyIORef' indat.gamepads $ case jsst of
        GLFW.JoystickState'Connected    -> HashMap.insert (fromEnum js) gs
        GLFW.JoystickState'Disconnected -> HashMap.delete (fromEnum js)
    Nothing -> pure ()

  case jsst of
    GLFW.JoystickState'Disconnected -> addInput $ InputGamePadConnection (fromEnum js) False
    _ -> pure ()

glfwTouchIdent :: GLFW.MouseButton -> Int
glfwTouchIdent button = case button of
  GLFW.MouseButton'1 -> 1
  GLFW.MouseButton'2 -> 2
  GLFW.MouseButton'3 -> 3
  GLFW.MouseButton'4 -> 4
  GLFW.MouseButton'5 -> 5
  GLFW.MouseButton'6 -> 6
  GLFW.MouseButton'7 -> 7
  GLFW.MouseButton'8 -> 8

touchPosToScreenPos :: (Double, Double) -> V2 Scalar
touchPosToScreenPos (x,y) = V2 (realToFrac x) (realToFrac y)
