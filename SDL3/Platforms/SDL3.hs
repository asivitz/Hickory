{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE CPP #-}

module Platforms.SDL3 where

import Control.Lens (_2, over)
import Data.IORef
import qualified Data.HashMap.Strict as HashMap
import Hickory.Types
import Hickory.Math.Vector
import Hickory.Input
import Data.Time
import Data.Maybe
import Linear (V2(..), (^*))
import Foreign (nullPtr)
import Data.HashMap.Strict (HashMap)
import Data.Traversable (for)
import qualified SDL3 as SDL
import qualified SDL3.Vulkan as SDL
import Data.Foldable (traverse_, for_)
import Acquire (Acquire)
import Hickory.Vulkan.Types (VulkanResources, Swapchain, FrameContext, runCleanup)
import Vulkan (Instance, SurfaceKHR(..), instanceHandle)
import Hickory.Vulkan.Vulkan (mkAcquire)
import Foreign (castPtr, Word8, Word16, Word32)
import Hickory.Vulkan.Utils (initVulkan, buildFrameFunction)
import Control.Monad.IO.Class (MonadIO(..))
import Vulkan.Extensions (destroySurfaceKHR)
import Control.Monad (void)
import Data.Functor ((<&>))
import qualified Data.Enum.Set as E


--TODO: RawInput Int should instead use a generic engine key type, and then
    --a method of converting GLFW.Key to it

data InputData = InputData
  { touches  :: IORef (HashMap Int (UTCTime, V2 Scalar))
  , keys     :: IORef (HashMap Key UTCTime)
  , gamepads :: IORef (HashMap Word32 (SDL.SDLGamepad, GamePad))
  }

data SDLHandles = SDLHandles
  { inputPoller  :: IO [RawInput]
  , shouldQuit   :: IO Bool
  , displayScale :: IO Scalar
  -- The controller
  -- Intensity of the low frequency (left) rumble motor
  -- Intensity of the high frequency (right) rumble motor
  -- Duration in ms
  , rumbleController :: Int ->  Word16 -> Word16 -> Word32 -> IO ()
  , setControllerLED :: Int -> Word8 -> Word8 -> Word8 -> IO ()
  }

getCurrentGamePadIds :: IO [Int]
getCurrentGamePadIds = undefined

touchPosToScreenPos :: (Double, Double) -> V2 Scalar
touchPosToScreenPos (x,y) = V2 (realToFrac x) (realToFrac y)

sdlFrameBuilder :: SDL.SDLWindow -> IO SDLHandles
sdlFrameBuilder win = sdlFrameBuilder' win pollEvents (pure False) (pure False)

pollEvents :: IO [SDL.SDLEvent]
pollEvents = go []
  where
  go es = do
    SDL.sdlPollEvent >>= \case
      Nothing -> pure es
      Just e -> go (e:es)

sdlFrameBuilder' :: SDL.SDLWindow -> IO [SDL.SDLEvent] -> IO Bool -> IO Bool -> IO SDLHandles
sdlFrameBuilder' win eventPoller wantCaptureMouse wantCaptureKeyboard = do
  joys <- SDL.sdlGetGamepads

  -- cds <- SDL.availableControllers
  gps <- for joys SDL.sdlOpenGamepad
  -- gcids <- for gcs \(SDL.GameController ptr) -> do
  --   js <- SDLRaw.gameControllerGetJoystick ptr
  --   SDLRaw.joystickInstanceID js
  let initialGamepads = HashMap.fromList . catMaybes $ zipWith (\(SDL.SDLJoystickID joy) mGp -> mGp <&> \gp -> (joy, (gp, emptyGamePad))) joys gps


  indat <- InputData
    <$> newIORef mempty
    <*> newIORef mempty
    <*> newIORef initialGamepads

  let initialConnectionEvents = HashMap.toList initialGamepads <&> \(i, _) -> InputGamePadConnection (fromIntegral i) True
  inputsRef <- newIORef initialConnectionEvents

  let addInput i = atomicModifyIORef' inputsRef \is -> (i:is, ())
  quitSignal <- newIORef False

  let rumbleController i left right dur = do
        HashMap.lookup (fromIntegral i) <$> readIORef indat.gamepads >>= traverse_ \(gp,_) -> do
          void $ SDL.sdlRumbleGamepad gp left right dur
      setControllerLED i r g b = do
        HashMap.lookup (fromIntegral i) <$> readIORef indat.gamepads >>= traverse_ \(gp,_) -> do
          void $ SDL.sdlSetGamepadLED gp r g b

  initialDisplayScale <- SDL.sdlGetWindowDisplayScale win
  displayScaleRef <- newIORef initialDisplayScale
  let displayScale = readIORef displayScaleRef

  initialPixelDensity <- SDL.sdlGetWindowPixelDensity win
  pixelDensityRef <- newIORef initialPixelDensity

  inputPoller <- pure do
    time <- getCurrentTime
    events <- eventPoller
    captureMouse <- wantCaptureMouse
    captureKeyboard <- wantCaptureKeyboard
    pixelDensity <- readIORef pixelDensityRef

    inputEvs <- catMaybes <$> for events \case
      SDL.SDLEventGamepadButton SDL.SDLGamepadButtonEvent {..} -> do
        let button = sdlGamepadButtonToGamePadButton sdlGamepadButtonButton
            SDL.SDLJoystickID joyid = sdlGamepadButtonWhich
            down = sdlGamepadButtonDown
        if down
        then do
          modifyIORef' indat.gamepads \mp -> HashMap.adjust (over (_2 . #buttons) $ E.union button) joyid mp
          pure . Just $ InputGamePadButtons Pressed (fromIntegral joyid) [button]
        else do
          modifyIORef' indat.gamepads \mp -> HashMap.adjust (over (_2 . #buttons) $ (E.\\ button)) joyid mp
          pure . Just $ InputGamePadButtons Released (fromIntegral joyid) [button]
      SDL.SDLEventGamepadDevice SDL.SDLGamepadDeviceEvent {..} -> do
        let SDL.SDLJoystickID joyid = sdlGamepadDeviceWhich
        case sdlGamepadDeviceType of
          SDL.SDL_EVENT_GAMEPAD_ADDED -> do
            SDL.sdlOpenGamepad sdlGamepadDeviceWhich >>= \case
              Nothing -> pure Nothing
              Just gp -> do
                modifyIORef' indat.gamepads $ HashMap.insert joyid (gp, emptyGamePad)
                pure $ Just $ InputGamePadConnection (fromIntegral joyid) True
          SDL.SDL_EVENT_GAMEPAD_REMOVED -> do
            mGp <- fmap fst . HashMap.lookup joyid <$> readIORef indat.gamepads
            for_ mGp \gp -> SDL.sdlCloseGamepad gp
            modifyIORef' indat.gamepads $ HashMap.delete joyid
            pure $ Just $ InputGamePadConnection (fromIntegral joyid) False
          _ -> pure Nothing
      SDL.SDLEventKeyboard SDL.SDLKeyboardEvent {..} | not captureKeyboard -> let key = sdlKeyToKey sdlKeyboardScancode in
        case sdlKeyboardType of
          SDL.SDL_EVENT_KEY_DOWN -> do
            modifyIORef' indat.keys $ HashMap.insert key time
            pure $ Just $ InputKeyDown key
          SDL.SDL_EVENT_KEY_UP -> do
            keys <- readIORef indat.keys
            let delta = case HashMap.lookup key keys of
                  Nothing -> 0
                  Just prev -> realToFrac $ diffUTCTime time prev
            modifyIORef' indat.keys $ HashMap.delete key
            pure $ Just $ InputKeyUp key delta
          _ -> pure Nothing
      SDL.SDLEventMouseMotion SDL.SDLMouseMotionEvent {..} | not captureMouse-> do
        touches <- readIORef indat.touches
        let location = V2 sdlMouseMotionX sdlMouseMotionY ^* pixelDensity
            ident = fromMaybe 0 $ listToMaybe $ HashMap.keys touches
        pure $ Just $ InputTouchesLoc [(location,ident)]
      SDL.SDLEventMouseButton SDL.SDLMouseButtonEvent {..} | not captureMouse -> do
        let
            location = V2 sdlMouseButtonX sdlMouseButtonY ^* pixelDensity
            ident = case fromIntegral sdlMouseButtonButton of
              SDL.SDL_BUTTON_LEFT -> 1
              SDL.SDL_BUTTON_RIGHT -> 2
              SDL.SDL_BUTTON_MIDDLE -> 3
              _ -> 4
        case sdlMouseButtonDown of
          False -> do
            touches <- readIORef indat.touches
            let (origLocation, duration) = case HashMap.lookup ident touches of
                  Just (origTime, origLoc) -> (origLoc, realToFrac (diffUTCTime time origTime))
                  Nothing -> (location, 0)
            modifyIORef' indat.touches $ HashMap.delete ident
            pure $ Just $ InputTouchesUp [PointUp {..}]
          True -> do
            modifyIORef' indat.touches $ HashMap.insert ident (time, location)
            pure $ Just $ InputTouchesDown [(location, ident)]
      _ -> pure Nothing

    modifyIORef' inputsRef (inputEvs++)

    for_ events \case
      SDL.SDLEventWindow we | we.sdlWindowType == SDL.SDL_EVENT_WINDOW_CLOSE_REQUESTED -> writeIORef quitSignal True
      SDL.SDLEventWindow we | we.sdlWindowType == SDL.SDL_EVENT_WINDOW_DISPLAY_SCALE_CHANGED -> do
        SDL.sdlGetWindowDisplayScale win >>= writeIORef displayScaleRef
      SDL.SDLEventWindow we | we.sdlWindowType == SDL.SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED -> do
        SDL.sdlGetWindowPixelDensity win >>= writeIORef pixelDensityRef
      SDL.SDLEventQuit _            -> writeIORef quitSignal True
      _ -> pure ()

    (HashMap.toList <$> readIORef indat.gamepads) >>= traverse_ \(gcid, (gc,gp)) -> do
      leftStick <- fmap (fmap $ \i -> rlerp (realToFrac i) (-32768) 32767 * 2 - 1) $
        V2 <$> SDL.sdlGetGamepadAxis gc SDL.SDL_GAMEPAD_AXIS_LEFTX
           <*> SDL.sdlGetGamepadAxis gc SDL.SDL_GAMEPAD_AXIS_LEFTY
      rightStick <- fmap (fmap $ \i -> rlerp (realToFrac i) (-32768) 32767 * 2 - 1) $
        V2 <$> SDL.sdlGetGamepadAxis gc SDL.SDL_GAMEPAD_AXIS_RIGHTX
           <*> SDL.sdlGetGamepadAxis gc SDL.SDL_GAMEPAD_AXIS_RIGHTY
      leftTrigger <- (\i -> realToFrac i / 32767 * 2 - 1) <$>
        SDL.sdlGetGamepadAxis gc SDL.SDL_GAMEPAD_AXIS_LEFT_TRIGGER
      rightTrigger <- (\i -> realToFrac i / 32767 * 2 - 1) <$>
        SDL.sdlGetGamepadAxis gc SDL.SDL_GAMEPAD_AXIS_RIGHT_TRIGGER

      let
        leftStickUp    = let V2 _x y = leftStick in y < -0.8
        leftStickRight = let V2 x _y = leftStick in x > 0.8
        leftStickDown  = let V2 _x y = leftStick in y > 0.8
        leftStickLeft  = let V2 x _y = leftStick in x < -0.8
        rightStickUp    = let V2 _x y = rightStick in y < -0.8
        rightStickRight = let V2 x _y = rightStick in x > 0.8
        rightStickDown  = let V2 _x y = rightStick in y > 0.8
        rightStickLeft  = let V2 x _y = rightStick in x < -0.8
        stickButtonPresses = E.fromFoldable $ catMaybes
          [ if leftStickUp && gp.leftStickUp == Released then Just LeftStickUp else Nothing
          , if leftStickRight && gp.leftStickRight == Released then Just LeftStickRight else Nothing
          , if leftStickDown && gp.leftStickDown == Released then Just LeftStickDown else Nothing
          , if leftStickLeft && gp.leftStickLeft == Released then Just LeftStickLeft else Nothing
          , if rightStickUp && gp.rightStickUp == Released then Just RightStickUp else Nothing
          , if rightStickRight && gp.rightStickRight == Released then Just RightStickRight else Nothing
          , if rightStickDown && gp.rightStickDown == Released then Just RightStickDown else Nothing
          , if rightStickLeft && gp.rightStickLeft == Released then Just RightStickLeft else Nothing
          ]
        stickButtonReleases = E.fromFoldable $ catMaybes
          [ if not leftStickUp && gp.leftStickUp == Pressed then Just LeftStickUp else Nothing
          , if not leftStickRight && gp.leftStickRight == Pressed then Just LeftStickRight else Nothing
          , if not leftStickDown && gp.leftStickDown == Pressed then Just LeftStickDown else Nothing
          , if not leftStickLeft && gp.leftStickLeft == Pressed then Just LeftStickLeft else Nothing
          , if not rightStickUp && gp.rightStickUp == Pressed then Just RightStickUp else Nothing
          , if not rightStickRight && gp.rightStickRight == Pressed then Just RightStickRight else Nothing
          , if not rightStickDown && gp.rightStickDown == Pressed then Just RightStickDown else Nothing
          , if not rightStickLeft && gp.rightStickLeft == Pressed then Just RightStickLeft else Nothing
          ]
        newGP = gp { leftStick, rightStick, leftTrigger, rightTrigger, buttons = E.union gp.buttons stickButtonPresses E.\\ stickButtonReleases  }

      modifyIORef' indat.gamepads $ HashMap.insert gcid (gc, newGP)

      addInput $ InputGamePad (fromIntegral gcid) newGP
      addInput $ InputGamePadButtons Pressed (fromIntegral gcid) [stickButtonPresses]
      addInput $ InputGamePadButtons Released (fromIntegral gcid) [stickButtonReleases]

    atomicModifyIORef' inputsRef (\is -> ([], reverse is))
  let shouldQuit = readIORef quitSignal
  pure SDLHandles {..}

sdlKeyToKey :: SDL.SDLScancode -> Key
sdlKeyToKey = \case
  SDL.SDL_SCANCODE_UNKNOWN -> Key'Unknown
  SDL.SDL_SCANCODE_A -> Key'A
  SDL.SDL_SCANCODE_B -> Key'B
  SDL.SDL_SCANCODE_C -> Key'C
  SDL.SDL_SCANCODE_D -> Key'D
  SDL.SDL_SCANCODE_E -> Key'E
  SDL.SDL_SCANCODE_F -> Key'F
  SDL.SDL_SCANCODE_G -> Key'G
  SDL.SDL_SCANCODE_H -> Key'H
  SDL.SDL_SCANCODE_I -> Key'I
  SDL.SDL_SCANCODE_J -> Key'J
  SDL.SDL_SCANCODE_K -> Key'K
  SDL.SDL_SCANCODE_L -> Key'L
  SDL.SDL_SCANCODE_M -> Key'M
  SDL.SDL_SCANCODE_N -> Key'N
  SDL.SDL_SCANCODE_O -> Key'O
  SDL.SDL_SCANCODE_P -> Key'P
  SDL.SDL_SCANCODE_Q -> Key'Q
  SDL.SDL_SCANCODE_R -> Key'R
  SDL.SDL_SCANCODE_S -> Key'S
  SDL.SDL_SCANCODE_T -> Key'T
  SDL.SDL_SCANCODE_U -> Key'U
  SDL.SDL_SCANCODE_V -> Key'V
  SDL.SDL_SCANCODE_W -> Key'W
  SDL.SDL_SCANCODE_X -> Key'X
  SDL.SDL_SCANCODE_Y -> Key'Y
  SDL.SDL_SCANCODE_Z -> Key'Z
  SDL.SDL_SCANCODE_1 -> Key'1
  SDL.SDL_SCANCODE_2 -> Key'2
  SDL.SDL_SCANCODE_3 -> Key'3
  SDL.SDL_SCANCODE_4 -> Key'4
  SDL.SDL_SCANCODE_5 -> Key'5
  SDL.SDL_SCANCODE_6 -> Key'6
  SDL.SDL_SCANCODE_7 -> Key'7
  SDL.SDL_SCANCODE_8 -> Key'8
  SDL.SDL_SCANCODE_9 -> Key'9
  SDL.SDL_SCANCODE_0 -> Key'0
  SDL.SDL_SCANCODE_RETURN -> Key'Enter
  SDL.SDL_SCANCODE_ESCAPE -> Key'Escape
  SDL.SDL_SCANCODE_BACKSPACE -> Key'Backspace
  SDL.SDL_SCANCODE_TAB -> Key'Tab
  SDL.SDL_SCANCODE_SPACE -> Key'Space
  SDL.SDL_SCANCODE_MINUS -> Key'Minus
  SDL.SDL_SCANCODE_EQUALS -> Key'Equal
  SDL.SDL_SCANCODE_LEFTBRACKET -> Key'LeftBracket
  SDL.SDL_SCANCODE_RIGHTBRACKET -> Key'RightBracket
  SDL.SDL_SCANCODE_BACKSLASH -> Key'Backslash
  -- ScancodeNonUSHash -> Key'NonUSHash
  SDL.SDL_SCANCODE_SEMICOLON -> Key'Semicolon
  SDL.SDL_SCANCODE_APOSTROPHE -> Key'Apostrophe
  SDL.SDL_SCANCODE_GRAVE -> Key'GraveAccent
  SDL.SDL_SCANCODE_COMMA -> Key'Comma
  SDL.SDL_SCANCODE_PERIOD -> Key'Period
  SDL.SDL_SCANCODE_SLASH -> Key'Slash
  SDL.SDL_SCANCODE_CAPSLOCK -> Key'CapsLock
  SDL.SDL_SCANCODE_F1 -> Key'F1
  SDL.SDL_SCANCODE_F2 -> Key'F2
  SDL.SDL_SCANCODE_F3 -> Key'F3
  SDL.SDL_SCANCODE_F4 -> Key'F4
  SDL.SDL_SCANCODE_F5 -> Key'F5
  SDL.SDL_SCANCODE_F6 -> Key'F6
  SDL.SDL_SCANCODE_F7 -> Key'F7
  SDL.SDL_SCANCODE_F8 -> Key'F8
  SDL.SDL_SCANCODE_F9 -> Key'F9
  SDL.SDL_SCANCODE_F10 -> Key'F10
  SDL.SDL_SCANCODE_F11 -> Key'F11
  SDL.SDL_SCANCODE_F12 -> Key'F12
  SDL.SDL_SCANCODE_PRINTSCREEN -> Key'PrintScreen
  SDL.SDL_SCANCODE_SCROLLLOCK -> Key'ScrollLock
  SDL.SDL_SCANCODE_PAUSE -> Key'Pause
  SDL.SDL_SCANCODE_INSERT -> Key'Insert
  SDL.SDL_SCANCODE_HOME -> Key'Home
  SDL.SDL_SCANCODE_PAGEUP -> Key'PageUp
  SDL.SDL_SCANCODE_DELETE -> Key'Delete
  SDL.SDL_SCANCODE_END -> Key'End
  SDL.SDL_SCANCODE_PAGEDOWN -> Key'PageDown
  SDL.SDL_SCANCODE_RIGHT -> Key'Right
  SDL.SDL_SCANCODE_LEFT -> Key'Left
  SDL.SDL_SCANCODE_DOWN -> Key'Down
  SDL.SDL_SCANCODE_UP -> Key'Up
  -- ScancodeNumLockClear -> Key'NumLockClear
  -- ScancodeKPDivide -> Key'KPDivide
  -- ScancodeKPMultiply -> Key'KPMultiply
  -- ScancodeKPMinus -> Key'KPMinus
  -- ScancodeKPPlus -> Key'KPPlus
  -- ScancodeKPEnter -> Key'KPEnter
  -- ScancodeKP1 -> Key'KP1
  -- ScancodeKP2 -> Key'KP2
  -- ScancodeKP3 -> Key'KP3
  -- ScancodeKP4 -> Key'KP4
  -- ScancodeKP5 -> Key'KP5
  -- ScancodeKP6 -> Key'KP6
  -- ScancodeKP7 -> Key'KP7
  -- ScancodeKP8 -> Key'KP8
  -- ScancodeKP9 -> Key'KP9
  -- ScancodeKP0 -> Key'KP0
  -- ScancodeKPPeriod -> Key'KPPeriod
  -- ScancodeNonUSBackslash -> Key'NonUSBackslash
  -- ScancodeApplication -> Key'Application
  -- ScancodePower -> Key'Power
  -- ScancodeKPEquals -> Key'KPEquals
  SDL.SDL_SCANCODE_F13 -> Key'F13
  SDL.SDL_SCANCODE_F14 -> Key'F14
  SDL.SDL_SCANCODE_F15 -> Key'F15
  SDL.SDL_SCANCODE_F16 -> Key'F16
  SDL.SDL_SCANCODE_F17 -> Key'F17
  SDL.SDL_SCANCODE_F18 -> Key'F18
  SDL.SDL_SCANCODE_F19 -> Key'F19
  SDL.SDL_SCANCODE_F20 -> Key'F20
  SDL.SDL_SCANCODE_F21 -> Key'F21
  SDL.SDL_SCANCODE_F22 -> Key'F22
  SDL.SDL_SCANCODE_F23 -> Key'F23
  SDL.SDL_SCANCODE_F24 -> Key'F24
  -- ScancodeExecute -> Key'Execute
  -- ScancodeHelp -> Key'Help
  -- ScancodeMenu -> Key'Menu
  -- ScancodeSelect -> Key'Select
  -- ScancodeStop -> Key'Stop
  -- ScancodeAgain -> Key'Again
  -- ScancodeUndo -> Key'Undo
  -- ScancodeCut -> Key'Cut
  -- ScancodeCopy -> Key'Copy
  -- ScancodePaste -> Key'Paste
  -- ScancodeFind -> Key'Find
  -- ScancodeMute -> Key'Mute
  -- ScancodeVolumeUp -> Key'VolumeUp
  -- ScancodeVolumeDown -> Key'VolumeDown
  -- ScancodeKPComma -> Key'KPComma
  -- ScancodeKPEqualsAS400 -> Key'KPEqualsAS400
  -- ScancodeInternational1 -> Key'International1
  -- ScancodeInternational2 -> Key'International2
  -- ScancodeInternational3 -> Key'International3
  -- ScancodeInternational4 -> Key'International4
  -- ScancodeInternational5 -> Key'International5
  -- ScancodeInternational6 -> Key'International6
  -- ScancodeInternational7 -> Key'International7
  -- ScancodeInternational8 -> Key'International8
  -- ScancodeInternational9 -> Key'International9
  -- ScancodeLang1 -> Key'Lang1
  -- ScancodeLang2 -> Key'Lang2
  -- ScancodeLang3 -> Key'Lang3
  -- ScancodeLang4 -> Key'Lang4
  -- ScancodeLang5 -> Key'Lang5
  -- ScancodeLang6 -> Key'Lang6
  -- ScancodeLang7 -> Key'Lang7
  -- ScancodeLang8 -> Key'Lang8
  -- ScancodeLang9 -> Key'Lang9
  -- ScancodeAltErase -> Key'AltErase
  -- ScancodeSysReq -> Key'SysReq
  -- ScancodeCancel -> Key'Cancel
  -- ScancodeClear -> Key'Clear
  -- ScancodePrior -> Key'Prior
  -- ScancodeReturn2 -> Key'Return2
  -- ScancodeSeparator -> Key'Separator
  -- ScancodeOut -> Key'Out
  -- ScancodeOper -> Key'Oper
  -- ScancodeClearAgain -> Key'ClearAgain
  -- ScancodeCrSel -> Key'CrSel
  -- ScancodeExSel -> Key'ExSel
  -- ScancodeKP00 -> Key'KP00
  -- ScancodeKP000 -> Key'KP000
  -- ScancodeThousandsSeparator -> Key'ThousandsSeparator
  -- ScancodeDecimalSeparator -> Key'DecimalSeparator
  -- ScancodeCurrencyUnit -> Key'CurrencyUnit
  -- ScancodeCurrencySubunit -> Key'CurrencySubunit
  -- ScancodeLeftParen -> Key'LeftParen
  -- ScancodeRightParen -> Key'RightParen
  -- ScancodeLeftBrace -> Key'LeftBrace
  -- ScancodeRightBrace -> Key'RightBrace
  -- ScancodeKPTab -> Key'KPTab
  -- ScancodeKPBackspace -> Key'KPBackspace
  -- ScancodeKPA -> Key'KPA
  -- ScancodeKPB -> Key'KPB
  -- ScancodeKPC -> Key'KPC
  -- ScancodeKPD -> Key'KPD
  -- ScancodeKPE -> Key'KPE
  -- ScancodeKPF -> Key'KPF
  -- ScancodeKPXOR -> Key'KPXOR
  -- ScancodeKPPower -> Key'KPPower
  -- ScancodeKPPercent -> Key'KPPercent
  -- ScancodeKPLess -> Key'KPLess
  -- ScancodeKPGreater -> Key'KPGreater
  -- ScancodeKPAmpersand -> Key'KPAmpersand
  -- ScancodeKPDblAmpersand -> Key'KPDblAmpersand
  -- ScancodeKPVerticalBar -> Key'KPVerticalBar
  -- ScancodeKPDblVerticalBar -> Key'KPDblVerticalBar
  -- ScancodeKPColon -> Key'KPColon
  -- ScancodeKPHash -> Key'KPHash
  -- ScancodeKPSpace -> Key'KPSpace
  -- ScancodeKPAt -> Key'KPAt
  -- ScancodeKPExclam -> Key'KPExclam
  -- ScancodeKPMemStore -> Key'KPMemStore
  -- ScancodeKPMemRecall -> Key'KPMemRecall
  -- ScancodeKPMemClear -> Key'KPMemClear
  -- ScancodeKPMemAdd -> Key'KPMemAdd
  -- ScancodeKPMemSubtract -> Key'KPMemSubtract
  -- ScancodeKPMemMultiply -> Key'KPMemMultiply
  -- ScancodeKPMemDivide -> Key'KPMemDivide
  -- ScancodeKPPlusMinus -> Key'KPPlusMinus
  -- ScancodeKPClear -> Key'KPClear
  -- ScancodeKPClearEntry -> Key'KPClearEntry
  -- ScancodeKPBinary -> Key'KPBinary
  -- ScancodeKPOctal -> Key'KPOctal
  -- ScancodeKPDecimal -> Key'KPDecimal
  -- ScancodeKPHexadecimal -> Key'KPHexadecimal
  SDL.SDL_SCANCODE_LCTRL -> Key'LeftControl
  SDL.SDL_SCANCODE_LSHIFT -> Key'LeftShift
  SDL.SDL_SCANCODE_LALT -> Key'LeftAlt
  SDL.SDL_SCANCODE_LGUI -> Key'LeftSuper
  SDL.SDL_SCANCODE_RCTRL -> Key'RightControl
  SDL.SDL_SCANCODE_RSHIFT -> Key'RightShift
  SDL.SDL_SCANCODE_RALT -> Key'RightAlt
  SDL.SDL_SCANCODE_RGUI -> Key'RightSuper
  -- ScancodeMode -> Key'Mode
  -- ScancodeAudioNext -> Key'AudioNext
  -- ScancodeAudioPrev -> Key'AudioPrev
  -- ScancodeAudioStop -> Key'AudioStop
  -- ScancodeAudioPlay -> Key'AudioPlay
  -- ScancodeAudioMute -> Key'AudioMute
  -- ScancodeMediaSelect -> Key'MediaSelect
  -- ScancodeWWW -> Key'WWW
  -- ScancodeMail -> Key'Mail
  -- ScancodeCalculator -> Key'Calculator
  -- ScancodeComputer -> Key'Computer
  -- ScancodeACSearch -> Key'ACSearch
  -- ScancodeACHome -> Key'ACHome
  -- ScancodeACBack -> Key'ACBack
  -- ScancodeACForward -> Key'ACForward
  -- ScancodeACStop -> Key'ACStop
  -- ScancodeACRefresh -> Key'ACRefresh
  -- ScancodeACBookmarks -> Key'ACBookmarks
  -- ScancodeBrightnessDown -> Key'BrightnessDown
  -- ScancodeBrightnessUp -> Key'BrightnessUp
  -- ScancodeDisplaySwitch -> Key'DisplaySwitch
  -- ScancodeKBDIllumToggle -> Key'KBDIllumToggle
  -- ScancodeKBDIllumDown -> Key'KBDIllumDown
  -- ScancodeKBDIllumUp -> Key'KBDIllumUp
  -- ScancodeEject -> Key'Eject
  -- ScancodeSleep -> Key'Sleep
  -- ScancodeApp1 -> Key'App1
  -- ScancodeApp2 -> Key'App2
  _ -> Key'Unknown

sdlGamepadButtonToGamePadButton :: SDL.SDLGamepadButton -> E.EnumSet GamePadButton
sdlGamepadButtonToGamePadButton = \case
  SDL.SDL_GAMEPAD_BUTTON_INVALID -> E.empty
  SDL.SDL_GAMEPAD_BUTTON_SOUTH -> E.singleton South
  SDL.SDL_GAMEPAD_BUTTON_EAST -> E.singleton East
  SDL.SDL_GAMEPAD_BUTTON_WEST -> E.singleton West
  SDL.SDL_GAMEPAD_BUTTON_NORTH -> E.singleton North
  SDL.SDL_GAMEPAD_BUTTON_BACK -> E.singleton Back
  SDL.SDL_GAMEPAD_BUTTON_GUIDE -> E.singleton Guide
  SDL.SDL_GAMEPAD_BUTTON_START -> E.singleton Start
  SDL.SDL_GAMEPAD_BUTTON_LEFT_STICK -> E.singleton LeftThumb
  SDL.SDL_GAMEPAD_BUTTON_RIGHT_STICK -> E.singleton RightThumb
  SDL.SDL_GAMEPAD_BUTTON_LEFT_SHOULDER -> E.singleton LeftBumper
  SDL.SDL_GAMEPAD_BUTTON_RIGHT_SHOULDER -> E.singleton RightBumper
  SDL.SDL_GAMEPAD_BUTTON_DPAD_UP -> E.singleton DPadUp
  SDL.SDL_GAMEPAD_BUTTON_DPAD_DOWN -> E.singleton DPadDown
  SDL.SDL_GAMEPAD_BUTTON_DPAD_LEFT -> E.singleton DPadLeft
  SDL.SDL_GAMEPAD_BUTTON_DPAD_RIGHT -> E.singleton DPadRight
  _ -> E.empty

withWindow :: Int -> Int -> String -> (SDL.SDLWindow -> IO ()) -> IO ()
withWindow width height title f = do
  _ <- SDL.sdlInit [SDL.SDL_INIT_VIDEO, SDL.SDL_INIT_EVENTS, SDL.SDL_INIT_GAMEPAD]

  -- PS4/PS5 gamepads need special hint to enable rumble
  _ <- SDL.sdlSetHint "SDL_HINT_JOYSTICK_ENHANCED_REPORTS" "1"

  mWindow <- SDL.sdlCreateWindow title (fromIntegral width) (fromIntegral height)
    [SDL.SDL_WINDOW_HIGH_PIXEL_DENSITY, SDL.SDL_WINDOW_RESIZABLE, SDL.SDL_WINDOW_VULKAN]

  for_ mWindow \window -> do
    f window

    SDL.sdlDestroyWindow window
    SDL.sdlQuit

withWindowSurface :: Instance -> SDL.SDLWindow -> Acquire SurfaceKHR
withWindowSurface inst window = mkAcquire create release
  where
  create = do
    SDL.sdlVulkanCreateSurface window (castPtr $ instanceHandle inst) nullPtr >>= \case
      Just surf -> pure (SurfaceKHR surf)
      Nothing -> error "Couldn't create surface"
  release surf = destroySurfaceKHR inst surf Nothing

initSDLVulkan :: SDL.SDLWindow -> Acquire VulkanResources
initSDLVulkan win = do
  reqExts <- liftIO SDL.sdlVulkanGetInstanceExtensions
  initVulkan reqExts (`withWindowSurface` win)

runFrames
  :: SDL.SDLWindow
  -> VulkanResources
  -> (Swapchain -> Acquire renderer) -- ^ Acquire renderer
  -> Acquire ((renderer -> FrameContext -> IO ()) -> IO ())
runFrames win vulkanResources acquireRenderer = do
  let queryPixelSize = do
        SDL.sdlGetWindowSizeInPixels win >>= \case
          Nothing -> error "Couldn't get window size"
          Just (x,y) -> pure $ Size x y
  exeFrame <- buildFrameFunction vulkanResources queryPixelSize acquireRenderer

  pure \f -> do
    exeFrame f
    runCleanup vulkanResources

windowDisplayScale :: SDL.SDLWindow -> IO Float
windowDisplayScale = SDL.sdlGetWindowDisplayScale
