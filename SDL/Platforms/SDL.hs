{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Platforms.SDL where

import Data.IORef
import qualified Data.HashMap.Strict as HashMap
import Hickory.Types
import Hickory.Math.Vector
import Hickory.Input
import Data.Time
import Data.Maybe
import Linear (V2(..))
import Data.HashMap.Strict (HashMap)
import Data.Traversable (for)
import qualified SDL
import qualified SDL.Input.GameController as SDL
import qualified SDL.Internal.Types as SDL
import qualified SDL.Raw as SDLRaw
import Data.Int (Int32)
import qualified Data.Vector as V
import Data.Foldable (traverse_, find, for_)
import Foreign.C.String (peekCString)
import SDL (WindowConfig(..), ControllerButtonEventData(..))
import Acquire.Acquire (Acquire)
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
import Data.Function (fix, (&))
import Hickory.ImGUI.ImGUI (renderDearImGui, initDearImGui)
import DearImGui.SDL.Vulkan (sdl2InitForVulkan)
import DearImGui.SDL (sdl2Shutdown, sdl2NewFrame)
import Control.Monad (void)
import Hickory.Vulkan.Renderer.Types (Scene)
import Hickory.GameLoop (gameLoop)
import Data.Functor ((<&>))
import Foreign.C (CInt)
import qualified Data.Enum.Set as E

--TODO: RawInput Int should instead use a generic engine key type, and then
    --a method of converting GLFW.Key to it

data InputData = InputData
  { touches  :: IORef (HashMap Int (UTCTime, V2 Scalar))
  , keys     :: IORef (HashMap Key UTCTime)
  , gamepads :: IORef (HashMap SDLRaw.JoystickID SDL.GameController)
  }

getCurrentGamePadIds :: IO [Int]
getCurrentGamePadIds = undefined

{-
mouseButtonCallback :: InputData -> (RawInput -> IO ()) -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> t -> IO ()
mouseButtonCallback indat addInput win button buttonState _modkeys = unlessM wantCaptureMouse do
  touches <- readIORef indat.touches

  curPos <- GLFW.getCursorPos win

  let pos = touchPosToScreenPos curPos
      touchid = glfwTouchIdent button

  (ev, touches') <- case buttonState of
    GLFW.MouseButtonState'Pressed -> do
      time <- getCurrentTime
      return (InputTouchesDown [(pos,touchid)], HashMap.insert touchid (time, pos) touches)
    GLFW.MouseButtonState'Released -> case HashMap.lookup touchid touches of
      Nothing -> return (InputTouchesUp [PointUp { duration = 0, location = pos, origLocation = pos, ident = touchid }], touches)
      Just (prevTime, prevPos) -> do
        time <- getCurrentTime
        let delta = realToFrac (diffUTCTime time prevTime)
        return (InputTouchesUp [PointUp { duration = delta, location = pos, origLocation = prevPos, ident = touchid }], HashMap.delete touchid touches)

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
  -}

touchPosToScreenPos :: (Double, Double) -> V2 Scalar
touchPosToScreenPos (x,y) = V2 (realToFrac x) (realToFrac y)

sdlFrameBuilder :: IO (IO InputFrame, IO Bool)
sdlFrameBuilder = do
  builder <- inputFrameBuilder

  cds <- SDL.availableControllers
  gcs <- for cds SDL.openController
  gcids <- for gcs \(SDL.GameController ptr) -> do
    js <- SDLRaw.gameControllerGetJoystick ptr
    SDLRaw.joystickInstanceID js
  -- let gcids = cds <&> fromIntegral . SDL.gameControllerDeviceId
  let initialGamepads = HashMap.fromList (zip (V.toList gcids) (V.toList gcs))


  indat <- InputData
    <$> newIORef mempty
    <*> newIORef mempty
    <*> newIORef initialGamepads

  let initialConnectionEvents = V.toList $ gcids <&> \i -> InputGamePadConnection (fromIntegral i) True
  inputsRef <- newIORef initialConnectionEvents

  let addInput i = atomicModifyIORef' inputsRef \is -> (i:is, ())
  -- GLFW.setMouseButtonCallback win $ Just (mouseButtonCallback indat addInput)
  -- GLFW.setKeyCallback win $ Just (keyCallback indat addInput)
  -- GLFW.setJoystickCallback $ Just (joystickCallback indat addInput)
  quitSignal <- newIORef False

  buildInp <- pure do
    -- touches <- readIORef indat.touches
    -- curPos <- GLFW.getCursorPos win
    -- let curloc = touchPosToScreenPos curPos
        -- ident = fromMaybe 0 $ listToMaybe $ HashMap.keys touches
    -- addInput $ InputTouchesLoc [(curloc,ident)]
    --
    events <- SDL.pollEvents
    inputEvs <- catMaybes <$> for events \case
          SDL.Event _ (SDL.ControllerButtonEvent SDL.ControllerButtonEventData {..}) -> pure . Just $
            InputGamePadButtons (sdlButtonStateToButtonState controllerButtonEventState) (fromIntegral controllerButtonEventWhich)
              [sdlButtonToGamePadButton controllerButtonEventButton]
          SDL.Event _ (SDL.ControllerDeviceEvent SDL.ControllerDeviceEventData {..}) -> do
            case controllerDeviceEventConnection of
              SDL.ControllerDeviceAdded   -> do
                gc <- SDLRaw.gameControllerOpen (fromIntegral controllerDeviceEventWhich) -- this is a device, not a joystick id
                js <- SDLRaw.gameControllerGetJoystick gc
                gcid <- SDLRaw.joystickInstanceID js
                modifyIORef' indat.gamepads $ HashMap.insert gcid (SDL.GameController gc)
                pure $ Just $ InputGamePadConnection (fromIntegral gcid) True
              SDL.ControllerDeviceRemoved -> do
                modifyIORef' indat.gamepads $ HashMap.delete (fromIntegral controllerDeviceEventWhich)
                pure $ Just $ InputGamePadConnection (fromIntegral controllerDeviceEventWhich) False
              SDL.ControllerDeviceRemapped -> pure Nothing
          _ -> pure Nothing

    modifyIORef' inputsRef (inputEvs++)

    for_ events \case
      SDL.Event _ (SDL.WindowClosedEvent _) -> writeIORef quitSignal True
      SDL.Event _ SDL.QuitEvent             -> writeIORef quitSignal True
      _ -> pure ()

    (HashMap.toList <$> readIORef indat.gamepads) >>= traverse_ \(gcid, gc) -> do
      let getButtonState = fmap toButton . SDL.controllerButton gc

      a <- getButtonState SDL.ControllerButtonA
      b <- getButtonState SDL.ControllerButtonB
      x <- getButtonState SDL.ControllerButtonX
      y <- getButtonState SDL.ControllerButtonY
      leftBumper <- getButtonState SDL.ControllerButtonLeftShoulder
      rightBumper <- getButtonState SDL.ControllerButtonRightShoulder
      back <- getButtonState SDL.ControllerButtonBack
      start <- getButtonState SDL.ControllerButtonStart
      guide <- getButtonState SDL.ControllerButtonGuide
      leftThumb <- getButtonState SDL.ControllerButtonLeftStick
      rightThumb <- getButtonState SDL.ControllerButtonRightStick
      dpadUp <- getButtonState SDL.ControllerButtonDpadUp
      dpadRight <- getButtonState SDL.ControllerButtonDpadRight
      dpadDown <- getButtonState SDL.ControllerButtonDpadDown
      dpadLeft <- getButtonState SDL.ControllerButtonDpadLeft
      let square = x
          triangle = y
          cross = a
          circle = b

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

      let gp = GamePad {..}


      addInput $ InputGamePad (fromIntegral gcid) gp

    -- writeIORef indat.gamepads (HashMap.fromList $ catMaybes newGamePads)
    atomicModifyIORef' inputsRef (\is -> ([], reverse is)) >>= builder
  pure (buildInp, readIORef quitSignal)
  where
  toButton = \case
    SDL.ControllerButtonPressed  -> Pressed
    SDL.ControllerButtonReleased -> Released
    SDL.ControllerButtonInvalidState -> Released -- error "Invalid button state"

sdlButtonToGamePadButton :: SDL.ControllerButton -> E.EnumSet GamePadButton
sdlButtonToGamePadButton = \case
  SDL.ControllerButtonInvalid -> error "Invalid sdl button"
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
  SDL.initialize [SDL.InitVideo, SDL.InitEvents, SDL.InitGameController]

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
  glfwReqExts <- liftIO $ SDL.vkGetInstanceExtensions win >>= mapM B.packCString
  initVulkan glfwReqExts (`withWindowSurface` win)

runFrames
  :: SDL.Window
  -> IO Bool
  -> VulkanResources
  -> (Swapchain -> Acquire renderer) -- ^ Acquire renderer
  -> (renderer -> FrameContext -> IO ()) -- ^ Execute a frame
  -> IO ()
runFrames win shouldQuit vulkanResources acquireRenderer f = do
  let imguiAcquire swap =
        (,) <$> initDearImGui (void $ sdl2InitForVulkan win) sdl2Shutdown vulkanResources swap
            <*> acquireRenderer swap
      imguiRender (imguiRes, userRes) frameContext = do
        renderDearImGui imguiRes frameContext sdl2NewFrame do
          f userRes frameContext

  -- TODO: Option to turn off dear-imgui?
  -- (exeFrame, cleanup) <- buildFrameFunction glfwReqExts (uncurry Size <$> GLFW.getFramebufferSize win) (`withWindowSurface` win) acquireUserResources f
  (exeFrame, cleanup) <- buildFrameFunction vulkanResources ((\(V2 x y) -> Size x y) . fmap fromIntegral <$> SDL.vkGetDrawableSize win) imguiAcquire imguiRender

  let
    runFrame = do
      -- events <- SDL.pollEvents
      exeFrame
      runCleanup vulkanResources
      shouldQuit

  fix \rec -> liftIO runFrame >>= \case
    False -> rec
    True -> pure ()
  liftIO cleanup

sdlGameLoop
  :: SDL.Window
  -> VulkanResources
  -> (Swapchain -> Acquire swapchainResources)
  -> NominalDiffTime
  -> ((Scene swapchainResources -> IO ()) -> IO (Scene swapchainResources))
  -> IO ()
sdlGameLoop win vulkanResources acquireSwapchainResources physicsTimeStep initialScene = do
  let scrSizeVar = SDL.windowSize win

  (frameBuilder, shouldQuit) <- sdlFrameBuilder
  gameLoop ((\(V2 x y) -> fmap fromIntegral (Size x y)) <$> SDL.get scrSizeVar) vulkanResources acquireSwapchainResources physicsTimeStep frameBuilder (runFrames win shouldQuit) initialScene
