{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels #-}

module Platforms.SDL where

import Control.Lens (_2, over)
import Data.IORef
import qualified Data.HashMap.Strict as HashMap
import Hickory.Types
import Hickory.Math.Vector
import Hickory.Input
import Data.Time
import Data.Maybe
import Linear (V2(..))
import Linear.Affine (Point(..))
import Data.HashMap.Strict (HashMap)
import Data.Traversable (for)
import qualified SDL
import qualified SDL.Input.GameController as SDL
import qualified SDL.Internal.Types as SDL
import qualified SDL.Raw as SDLRaw
import qualified Data.Vector as V
import Data.Foldable (traverse_, for_)
import SDL (WindowConfig(..), ControllerButtonEventData(..))
import Acquire (Acquire)
import Hickory.Vulkan.Types (VulkanResources, Swapchain, FrameContext, runCleanup)
import Vulkan (Instance, SurfaceKHR(..), instanceHandle)
import Data.Text (pack)
import Hickory.Vulkan.Vulkan (mkAcquire)
import Foreign (castPtr)
import qualified SDL.Video.Vulkan as SDL
import Hickory.Vulkan.Utils (initVulkan, buildFrameFunction)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.ByteString as B
import Vulkan.Extensions (destroySurfaceKHR)
import Hickory.ImGUI.ImGUI (renderDearImGui, initDearImGui)
import DearImGui.SDL.Vulkan (sdl2InitForVulkan)
import DearImGui.SDL (sdl2Shutdown, sdl2NewFrame, pollEventsWithImGui)
import Control.Monad (void)
import Data.Functor ((<&>))
import Foreign.C (CUShort, CUInt)
import qualified Data.Enum.Set as E
import SDL.Input.Keyboard.Codes
import Data.Text.Foreign (withCString)
import DearImGui (wantCaptureMouse, wantCaptureKeyboard)

--TODO: RawInput Int should instead use a generic engine key type, and then
    --a method of converting GLFW.Key to it

data InputData = InputData
  { touches  :: IORef (HashMap Int (UTCTime, V2 Scalar))
  , keys     :: IORef (HashMap Key UTCTime)
  , gamepads :: IORef (HashMap SDLRaw.JoystickID (SDL.GameController, GamePad))
  }

data SDLHandles = SDLHandles
  { inputPoller :: IO [RawInput]
  , shouldQuit  :: IO Bool
  -- The controller
  -- Intensity of the low frequency (left) rumble motor
  -- Intensity of the high frequency (right) rumble motor
  -- Duration in ms
  , rumbleController :: Int -> CUShort -> CUShort -> CUInt -> IO ()
  }

getCurrentGamePadIds :: IO [Int]
getCurrentGamePadIds = undefined

touchPosToScreenPos :: (Double, Double) -> V2 Scalar
touchPosToScreenPos (x,y) = V2 (realToFrac x) (realToFrac y)

sdlFrameBuilder :: IO SDLHandles
sdlFrameBuilder = do
  cds <- SDL.availableControllers
  gcs <- for cds SDL.openController
  gcids <- for gcs \(SDL.GameController ptr) -> do
    js <- SDLRaw.gameControllerGetJoystick ptr
    SDLRaw.joystickInstanceID js
  let initialGamepads = HashMap.fromList (zip (V.toList gcids) ((,emptyGamePad) <$> V.toList gcs))


  indat <- InputData
    <$> newIORef mempty
    <*> newIORef mempty
    <*> newIORef initialGamepads

  let initialConnectionEvents = V.toList $ gcids <&> \i -> InputGamePadConnection (fromIntegral i) True
  inputsRef <- newIORef initialConnectionEvents

  let addInput i = atomicModifyIORef' inputsRef \is -> (i:is, ())
  quitSignal <- newIORef False

  let rumbleController i left right dur = do
        HashMap.lookup (fromIntegral i) <$> readIORef indat.gamepads >>= traverse_ \(SDL.GameController ptr,_) -> do
          void $ SDLRaw.gameControllerRumble ptr left right dur

  inputPoller <- pure do
    time <- getCurrentTime
    events <- pollEventsWithImGui
    captureMouse <- wantCaptureMouse
    captureKeyboard <- wantCaptureKeyboard
    inputEvs <- catMaybes <$> for events \(SDL.Event _ payload) -> case payload of
          SDL.ControllerButtonEvent SDL.ControllerButtonEventData {..} -> do
            let bs = sdlButtonStateToButtonState controllerButtonEventState
                cntrlr = fromIntegral controllerButtonEventWhich
                button = sdlButtonToGamePadButton controllerButtonEventButton

            modifyIORef' indat.gamepads \mp -> case bs of
              Pressed  -> HashMap.adjust (over (_2 . #buttons) $ E.union button) (fromIntegral cntrlr) mp
              Released -> HashMap.adjust (over (_2 . #buttons) $ (E.\\ button)) (fromIntegral cntrlr) mp
            pure . Just $ InputGamePadButtons bs cntrlr [button]
          SDL.ControllerDeviceEvent SDL.ControllerDeviceEventData {..} -> do
            case controllerDeviceEventConnection of
              SDL.ControllerDeviceAdded   -> do
                gc <- SDLRaw.gameControllerOpen (fromIntegral controllerDeviceEventWhich) -- this is a device, not a joystick id
                js <- SDLRaw.gameControllerGetJoystick gc
                gcid <- SDLRaw.joystickInstanceID js
                modifyIORef' indat.gamepads $ HashMap.insert gcid (SDL.GameController gc, emptyGamePad)
                pure $ Just $ InputGamePadConnection (fromIntegral gcid) True
              SDL.ControllerDeviceRemoved -> do
                gc <- fmap fst . HashMap.lookup controllerDeviceEventWhich <$> readIORef indat.gamepads
                for_ gc \(SDL.GameController ptr) -> SDLRaw.gameControllerClose ptr
                modifyIORef' indat.gamepads $ HashMap.delete controllerDeviceEventWhich
                pure $ Just $ InputGamePadConnection (fromIntegral controllerDeviceEventWhich) False
              SDL.ControllerDeviceRemapped -> pure Nothing
          SDL.KeyboardEvent SDL.KeyboardEventData {..} | not captureKeyboard -> let key = sdlKeyToKey keyboardEventKeysym in
            case keyboardEventKeyMotion of
              SDL.Pressed  -> do
                modifyIORef' indat.keys $ HashMap.insert key time
                pure $ Just $ InputKeyDown key
              SDL.Released -> do
                keys <- readIORef indat.keys
                let delta = case HashMap.lookup key keys of
                      Nothing -> 0
                      Just prev -> realToFrac $ diffUTCTime time prev
                modifyIORef' indat.keys $ HashMap.delete key
                pure $ Just $ InputKeyUp key delta
          SDL.MouseMotionEvent SDL.MouseMotionEventData {mouseMotionEventPos = (P v)} | not captureMouse-> do
            touches <- readIORef indat.touches
            let location = fmap realToFrac v
                ident = fromMaybe 0 $ listToMaybe $ HashMap.keys touches
            pure $ Just $ InputTouchesLoc [(location,ident)]
          SDL.MouseButtonEvent SDL.MouseButtonEventData {mouseButtonEventPos = (P v), ..} | not captureMouse -> do
            let location = fmap realToFrac v
                ident = case mouseButtonEventButton of
                  SDL.ButtonLeft -> 1
                  SDL.ButtonRight -> 2
                  SDL.ButtonMiddle -> 3
                  _ -> 4
            case mouseButtonEventMotion of
              SDL.Released -> do
                touches <- readIORef indat.touches
                let (origLocation, duration) = case HashMap.lookup ident touches of
                      Just (origTime, origLoc) -> (origLoc, realToFrac (diffUTCTime time origTime))
                      Nothing -> (location, 0)
                modifyIORef' indat.touches $ HashMap.delete ident
                pure $ Just $ InputTouchesUp [PointUp {..}]
              SDL.Pressed -> do
                modifyIORef' indat.touches $ HashMap.insert ident (time, location)
                pure $ Just $ InputTouchesDown [(location, ident)]
          _ -> pure Nothing

    modifyIORef' inputsRef (inputEvs++)

    for_ events \case
      SDL.Event _ (SDL.WindowClosedEvent _) -> writeIORef quitSignal True
      SDL.Event _ SDL.QuitEvent             -> writeIORef quitSignal True
      _ -> pure ()

    (HashMap.toList <$> readIORef indat.gamepads) >>= traverse_ \(gcid, (gc,gp)) -> do
      leftStick <- fmap (fmap $ \i -> rlerp (realToFrac i) (-32768) 32767 * 2 - 1) $
        V2 <$> SDL.controllerAxis gc SDL.ControllerAxisLeftX
           <*> SDL.controllerAxis gc SDL.ControllerAxisLeftY
      rightStick <- fmap (fmap $ \i -> rlerp (realToFrac i) (-32768) 32767 * 2 - 1) $
        V2 <$> SDL.controllerAxis gc SDL.ControllerAxisRightX
           <*> SDL.controllerAxis gc SDL.ControllerAxisRightY
      leftTrigger <- (\i -> realToFrac i / 32767 * 2 - 1) <$>
        SDL.controllerAxis gc SDL.ControllerAxisTriggerLeft
      rightTrigger <- (\i -> realToFrac i / 32767 * 2 - 1) <$>
        SDL.controllerAxis gc SDL.ControllerAxisTriggerRight

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
  where
  toButton = \case
    SDL.ControllerButtonPressed  -> Pressed
    SDL.ControllerButtonReleased -> Released
    SDL.ControllerButtonInvalidState -> Released -- error "Invalid button state"

sdlKeyToKey :: SDL.Keysym -> Key
sdlKeyToKey SDL.Keysym {..} = case keysymScancode of
  ScancodeUnknown -> Key'Unknown
  ScancodeA -> Key'A
  ScancodeB -> Key'B
  ScancodeC -> Key'C
  ScancodeD -> Key'D
  ScancodeE -> Key'E
  ScancodeF -> Key'F
  ScancodeG -> Key'G
  ScancodeH -> Key'H
  ScancodeI -> Key'I
  ScancodeJ -> Key'J
  ScancodeK -> Key'K
  ScancodeL -> Key'L
  ScancodeM -> Key'M
  ScancodeN -> Key'N
  ScancodeO -> Key'O
  ScancodeP -> Key'P
  ScancodeQ -> Key'Q
  ScancodeR -> Key'R
  ScancodeS -> Key'S
  ScancodeT -> Key'T
  ScancodeU -> Key'U
  ScancodeV -> Key'V
  ScancodeW -> Key'W
  ScancodeX -> Key'X
  ScancodeY -> Key'Y
  ScancodeZ -> Key'Z
  Scancode1 -> Key'1
  Scancode2 -> Key'2
  Scancode3 -> Key'3
  Scancode4 -> Key'4
  Scancode5 -> Key'5
  Scancode6 -> Key'6
  Scancode7 -> Key'7
  Scancode8 -> Key'8
  Scancode9 -> Key'9
  Scancode0 -> Key'0
  ScancodeReturn -> Key'Enter
  ScancodeEscape -> Key'Escape
  ScancodeBackspace -> Key'Backspace
  ScancodeTab -> Key'Tab
  ScancodeSpace -> Key'Space
  ScancodeMinus -> Key'Minus
  ScancodeEquals -> Key'Equal
  ScancodeLeftBracket -> Key'LeftBracket
  ScancodeRightBracket -> Key'RightBracket
  ScancodeBackslash -> Key'Backslash
  -- ScancodeNonUSHash -> Key'NonUSHash
  ScancodeSemicolon -> Key'Semicolon
  ScancodeApostrophe -> Key'Apostrophe
  ScancodeGrave -> Key'GraveAccent
  ScancodeComma -> Key'Comma
  ScancodePeriod -> Key'Period
  ScancodeSlash -> Key'Slash
  ScancodeCapsLock -> Key'CapsLock
  ScancodeF1 -> Key'F1
  ScancodeF2 -> Key'F2
  ScancodeF3 -> Key'F3
  ScancodeF4 -> Key'F4
  ScancodeF5 -> Key'F5
  ScancodeF6 -> Key'F6
  ScancodeF7 -> Key'F7
  ScancodeF8 -> Key'F8
  ScancodeF9 -> Key'F9
  ScancodeF10 -> Key'F10
  ScancodeF11 -> Key'F11
  ScancodeF12 -> Key'F12
  ScancodePrintScreen -> Key'PrintScreen
  ScancodeScrollLock -> Key'ScrollLock
  ScancodePause -> Key'Pause
  ScancodeInsert -> Key'Insert
  ScancodeHome -> Key'Home
  ScancodePageUp -> Key'PageUp
  ScancodeDelete -> Key'Delete
  ScancodeEnd -> Key'End
  ScancodePageDown -> Key'PageDown
  ScancodeRight -> Key'Right
  ScancodeLeft -> Key'Left
  ScancodeDown -> Key'Down
  ScancodeUp -> Key'Up
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
  ScancodeF13 -> Key'F13
  ScancodeF14 -> Key'F14
  ScancodeF15 -> Key'F15
  ScancodeF16 -> Key'F16
  ScancodeF17 -> Key'F17
  ScancodeF18 -> Key'F18
  ScancodeF19 -> Key'F19
  ScancodeF20 -> Key'F20
  ScancodeF21 -> Key'F21
  ScancodeF22 -> Key'F22
  ScancodeF23 -> Key'F23
  ScancodeF24 -> Key'F24
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
  ScancodeLCtrl -> Key'LeftControl
  ScancodeLShift -> Key'LeftShift
  ScancodeLAlt -> Key'LeftAlt
  ScancodeLGUI -> Key'LeftSuper
  ScancodeRCtrl -> Key'RightControl
  ScancodeRShift -> Key'RightShift
  ScancodeRAlt -> Key'RightAlt
  ScancodeRGUI -> Key'RightSuper
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

sdlButtonToGamePadButton :: SDL.ControllerButton -> E.EnumSet GamePadButton
sdlButtonToGamePadButton = \case
  SDL.ControllerButtonInvalid -> E.empty
  SDL.ControllerButtonA -> E.fromFoldable [A, Cross]
  SDL.ControllerButtonB -> E.fromFoldable [B, Circle]
  SDL.ControllerButtonX -> E.fromFoldable [X, Square]
  SDL.ControllerButtonY -> E.fromFoldable [Y, Triangle]
  SDL.ControllerButtonBack -> E.singleton Back
  SDL.ControllerButtonGuide -> E.singleton Guide
  SDL.ControllerButtonStart -> E.singleton Start
  SDL.ControllerButtonLeftStick -> E.singleton LeftThumb
  SDL.ControllerButtonRightStick -> E.singleton RightThumb
  SDL.ControllerButtonLeftShoulder -> E.singleton LeftBumper
  SDL.ControllerButtonRightShoulder -> E.singleton RightBumper
  SDL.ControllerButtonDpadUp -> E.singleton DPadUp
  SDL.ControllerButtonDpadDown -> E.singleton DPadDown
  SDL.ControllerButtonDpadLeft -> E.singleton DPadLeft
  SDL.ControllerButtonDpadRight -> E.singleton DPadRight

sdlButtonStateToButtonState :: SDL.ControllerButtonState -> ButtonState
sdlButtonStateToButtonState = \case
  SDL.ControllerButtonPressed -> Pressed
  SDL.ControllerButtonReleased -> Released
  SDL.ControllerButtonInvalidState -> error "Unknown sdl button state"

withWindow :: Int -> Int -> String -> (SDL.Window -> IO ()) -> IO ()
withWindow width height title f = do
  SDL.initialize [SDL.InitVideo, SDL.InitEvents, SDL.InitGameController, SDL.InitHaptic]

  -- These controllers need special hints to enable rumble
  withCString "SDL_JOYSTICK_HIDAPI_PS4_RUMBLE" \cs -> withCString "1" \arg -> SDLRaw.setHint cs arg
  withCString "SDL_JOYSTICK_HIDAPI_PS5_RUMBLE" \cs -> withCString "1" \arg -> SDLRaw.setHint cs arg

  window <- SDL.createWindow (pack title) $ WindowConfig
    { windowBorder          = True
    , windowHighDPI         = True
    , windowInputGrabbed    = False
    , windowMode            = SDL.Windowed
    , windowGraphicsContext = SDL.VulkanContext
    , windowPosition        = SDL.Wherever
    , windowResizable       = True
    , windowInitialSize     = V2 (fromIntegral width) (fromIntegral height)
    , windowVisible         = True
    }

  f window

  SDL.destroyWindow window
  SDL.quit
  where
  simpleErrorCallback e s = putStrLn $ unwords [show e, show s]

withWindowSurface :: Instance -> SDL.Window -> Acquire SurfaceKHR
withWindowSurface inst window = mkAcquire create release
  where
  create = SurfaceKHR <$> SDL.vkCreateSurface window (castPtr $ instanceHandle inst)
  release surf = destroySurfaceKHR inst surf Nothing

initSDLVulkan :: SDL.Window -> Acquire VulkanResources
initSDLVulkan win = do
  reqExts <- liftIO $ SDL.vkGetInstanceExtensions win >>= mapM B.packCString
  initVulkan reqExts (`withWindowSurface` win)

runFrames
  :: SDL.Window
  -> VulkanResources
  -> (Swapchain -> Acquire renderer) -- ^ Acquire renderer
  -> Acquire ((renderer -> FrameContext -> IO ()) -> IO ())
runFrames win vulkanResources acquireRenderer = do
  let imguiAcquire swap =
        (,) <$> initDearImGui (void $ sdl2InitForVulkan win) sdl2Shutdown vulkanResources swap
            <*> acquireRenderer swap
      imguiRender f (imguiRes, userRes) frameContext = do
        renderDearImGui imguiRes frameContext sdl2NewFrame do
          f userRes frameContext

  -- TODO: Option to turn off dear-imgui?
  exeFrame <- buildFrameFunction vulkanResources ((\(V2 x y) -> Size x y) . fmap fromIntegral <$> SDL.vkGetDrawableSize win) imguiAcquire

  pure \f -> do
    exeFrame (imguiRender f)
    runCleanup vulkanResources

sdlScreenSize :: MonadIO f => SDL.Window -> f (Size Int)
sdlScreenSize win = ((\(V2 x y) -> fmap fromIntegral (Size x y)) <$> SDL.get (SDL.windowSize win))
