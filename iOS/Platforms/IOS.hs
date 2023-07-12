{-# LANGUAGE ForeignFunctionInterface, BlockArguments, ViewPatterns #-}
{-# LANGUAGE TupleSections, PatternSynonyms, OverloadedLists, RecordWildCards #-}

module Platforms.IOS where

import Data.IORef
import Data.Time
import Foreign
import Foreign.C
import Hickory.Input
import Hickory.Math.Vector
import System.Mem (performMinorGC)
import qualified Data.HashMap.Strict as HashMap
import Hickory.Types
import Hickory.Platform
import Linear.V2 (V2(V2))
import Hickory.FRP.CoreEvents (CoreEventGenerators, coreEventGenerators)
import Hickory.Vulkan.Vulkan (unWrapAcquire)
import Vulkan (MetalSurfaceCreateInfoEXT(..), CAMetalLayer, createMetalSurfaceEXT, pattern EXT_METAL_SURFACE_EXTENSION_NAME)
import Vulkan.Zero (Zero(..))
import Hickory.Vulkan.Utils (buildFrameFunction, initVulkan)
import Acquire.Acquire (Acquire)
import Hickory.Vulkan.Types (VulkanResources(..), Swapchain (..), FrameContext (..))
import Control.Monad.IO.Class (liftIO)
import Data.Bool (bool)
import Data.Foldable (for_)

foreign import ccall "getResourcePath" c'getResourcePath :: CString -> CInt -> IO ()

foreign export ccall "touch_began" touchBegan :: StablePtr (a,InputData) -> CInt -> CDouble -> CDouble -> IO ()
foreign export ccall "touch_moved" touchMoved :: StablePtr (a,InputData) -> CInt -> CDouble -> CDouble -> IO ()
foreign export ccall "touch_ended" touchEnded :: StablePtr (a,InputData) -> CInt -> CDouble -> CDouble -> IO ()
foreign export ccall "draw"        draw       :: StablePtr (IO (),a,IO ()) -> IO ()
foreign export ccall "gamepad_state" gamepadState
  :: StablePtr (a,InputData)
  -> CInt -- Gamepad Ident
  -> CBool -- Button A
  -> CBool -- Button B
  -> CBool -- Button X
  -> CBool -- Button Y
  -> CBool -- Button RightTrigger
  -> CBool -- Button LeftTrigger
  -> CDouble -- RightShoulder
  -> CDouble -- LeftShoulder
  -> CDouble -- RightThumbXAxis
  -> CDouble -- RightThumbYAxis
  -> CBool -- RightThumb Button
  -> CDouble -- LeftThumbXAxis
  -> CDouble -- LeftThumbYAxis
  -> CBool -- LeftThumb Button
  -> CBool -- Button Menu
  -> CBool -- Button Options
  -> IO ()
foreign export ccall "gamepad_connected" gamepadConnected :: StablePtr (a,InputData) -> CInt -> IO ()
foreign export ccall "gamepad_disconnected" gamepadDisconnected :: StablePtr (a,InputData) -> CInt -> IO ()

type CDrawInit = Ptr CAMetalLayer -> CInt -> CInt -> IO (StablePtr (IO (), InputData, IO ()))

type RenderInit renderer
  =  VulkanResources
  -> Swapchain
  -> Acquire renderer

type DrawInit renderer gamedata
  =  VulkanResources
  -> CoreEventGenerators (renderer, FrameContext)
  -> Acquire ()

initIOSVulkan :: Ptr CAMetalLayer -> Acquire VulkanResources
initIOSVulkan layerPtr = do
  let reqExts = [EXT_METAL_SURFACE_EXTENSION_NAME]
      mkSurface instnce = createMetalSurfaceEXT instnce zero { layer = layerPtr } Nothing
  initVulkan reqExts mkSurface

mkDrawInit
  :: RenderInit renderer        -- Creates renderer
  -> DrawInit renderer gamedata -- Initializes logic/draw system
  -> CDrawInit
mkDrawInit ri di layerPtr w h = do
  td           <- initTouchData
  inputPoller  <- makeInputPoller (touchFunc td)
  timePoller   <- makeTimePoller
  wSizeRef     <- newIORef (Size (fromIntegral w) (fromIntegral h))

  (coreEvProc, evGens) <- coreEventGenerators inputPoller timePoller wSizeRef
  ((exeFrame, cleanupUserRes), cleanupVulkan)
    <- unWrapAcquire do
      vulkanResources <- initIOSVulkan layerPtr
      di vulkanResources evGens
      liftIO $ buildFrameFunction vulkanResources (pure $ Size (fromIntegral w) (fromIntegral h)) (ri vulkanResources)
                          \renderer frameContext -> do
                            coreEvProc (renderer, frameContext)

  newStablePtr (exeFrame, td, cleanupUserRes >> cleanupVulkan)

resourcesPath :: IO String
resourcesPath = do
  ptr <- mallocArray 1024
  c'getResourcePath ptr 1024
  str <- peekCString ptr
  free ptr
  return str

draw :: StablePtr (IO (), a, IO ()) -> IO ()
draw pkg = do
  (drawF,_,_) <- deRefStablePtr pkg
  drawF
  performMinorGC

data InputData = InputData
  { newDowns    :: IORef [(Int, Scalar, Scalar, UTCTime)]
  , newUps      :: IORef [(Int, Scalar, Scalar, UTCTime)]
  , newMoves    :: IORef [(Int, Scalar, Scalar)]
  , touches     :: IORef (HashMap.HashMap Int (V2 Scalar, UTCTime, V2 Scalar)) -- Current loc, origin time, origin loc
  , newRawMessages :: IORef [RawInput]
  }

initTouchData :: IO InputData
initTouchData = InputData <$> newIORef [] <*> newIORef [] <*> newIORef [] <*> newIORef HashMap.empty <*> newIORef []

touchBegan :: StablePtr (a, InputData) -> CInt -> CDouble -> CDouble -> IO ()
touchBegan touchData ident x y = do
  (_, InputData {..}) <- deRefStablePtr touchData
  time <- getCurrentTime
  modifyIORef newDowns ((fromIntegral ident, realToFrac x, realToFrac y, time):)

touchMoved :: StablePtr (a, InputData) -> CInt -> CDouble -> CDouble -> IO ()
touchMoved touchData ident x y = do
  (_, InputData {..}) <- deRefStablePtr touchData
  modifyIORef newMoves ((fromIntegral ident, realToFrac x, realToFrac y):)

touchEnded :: StablePtr (a, InputData) -> CInt -> CDouble -> CDouble -> IO ()
touchEnded touchData ident x y = do
  (_, InputData {..}) <- deRefStablePtr touchData
  time <- getCurrentTime
  modifyIORef newUps ((fromIntegral ident, realToFrac x, realToFrac y, time):)

touchFunc :: InputData -> (RawInput -> IO ()) -> IO (IO ())
touchFunc touchData handleRawInput = pure $ do
  let InputData {..} = touchData

  curhash <- readIORef touches
  moves <- atomicModifyIORef newMoves ([],)
  ups <- atomicModifyIORef newUps ([],)
  downs <- atomicModifyIORef newDowns ([],)
  rawMessages <- atomicModifyIORef newRawMessages ([],)

  -- Update the touch positions for move events
  let hash' = foldl (\hsh (ident, x, y) -> HashMap.adjust (\(_, time, origv) -> (V2 x y, time, origv)) ident hsh) curhash moves

  -- Broadcast a loc event for touches held over from last frame
  handleRawInput (InputTouchesLoc (map (\(ident, (loc', _, origv)) -> (loc', ident)) (HashMap.toList hash')))

  -- Add new touches
  handleRawInput (InputTouchesDown (map (\(ident, x, y, _time) -> (V2 x y, ident)) downs))
  let hash'' = foldl (\hsh (ident, x, y, time) -> HashMap.insert ident (V2 x y, time, V2 x y) hsh) hash' downs

  -- Remove released touches
  handleRawInput (InputTouchesUp (map (\(ident, x, y, time) ->
                                                          case HashMap.lookup ident hash'' of
                                                              Nothing -> PointUp { duration = 0, location = V2 x y, origLocation = V2 x y, ident = ident }
                                                              Just (_, prev,origv) ->
                                                                PointUp { duration = realToFrac (diffUTCTime time prev), location = V2 x y, origLocation = origv, ident = ident })
                                                      ups))
  let hash''' = foldl (\hsh (ident, _x, _y, _time) -> HashMap.delete ident hsh) hash'' ups

  -- Record the new hash
  writeIORef touches hash'''

  for_ rawMessages handleRawInput

gamepadState
  :: StablePtr (a,InputData)
  -> CInt -- Gamepad Ident
  -> CBool -- Button A
  -> CBool -- Button B
  -> CBool -- Button X
  -> CBool -- Button Y
  -> CBool -- Button RightShoulder
  -> CBool -- Button LeftShoulder
  -> CDouble -- RightTrigger
  -> CDouble -- LeftTrigger
  -> CDouble -- RightThumbXAxis
  -> CDouble -- RightThumbYAxis
  -> CBool -- RightThumb Button
  -> CDouble -- LeftThumbXAxis
  -> CDouble -- LeftThumbYAxis
  -> CBool -- LeftThumb Button
  -> CBool -- Button Menu
  -> CBool -- Button Options
  -> IO ()
gamepadState touchData ident a' b' x' y' rightShoulder' leftShoulder' (realToFrac -> rightTrigger) (realToFrac -> leftTrigger)
    rightThumbXAxis rightThumbYAxis rightThumb'
    leftThumbXAxis leftThumbYAxis leftThumb'
    menu options = do
  let leftStick = realToFrac <$> V2 leftThumbXAxis (negate leftThumbYAxis)
      rightStick = realToFrac <$> V2 rightThumbXAxis (negate rightThumbYAxis)
      back = mkPress options
      start = mkPress menu
      guide = Released
      a = mkPress a'
      b = mkPress b'
      x = mkPress x'
      y = mkPress y'
      cross = a
      circle = b
      square = x
      triangle = y
      rightBumper = mkPress rightShoulder'
      leftBumper = mkPress leftShoulder'
      rightThumb = mkPress rightThumb'
      leftThumb = mkPress leftThumb'
      dpadUp = Released
      dpadRight = Released
      dpadLeft = Released
      dpadDown = Released

  (_, InputData {..}) <- deRefStablePtr touchData
  modifyIORef newRawMessages (InputGamePad (fromIntegral ident) GamePad {..}:)
  where
  mkPress = bool Released Pressed . (/=0)

gamepadConnected :: StablePtr (a,InputData) -> CInt -> IO ()
gamepadConnected inputData ident = do
  (_, InputData {..}) <- deRefStablePtr inputData
  modifyIORef newRawMessages (InputGamePadConnection (fromIntegral ident) True :)

gamepadDisconnected :: StablePtr (a,InputData) -> CInt -> IO ()
gamepadDisconnected inputData ident = do
  (_, InputData {..}) <- deRefStablePtr inputData
  modifyIORef newRawMessages (InputGamePadConnection (fromIntegral ident) False :)
