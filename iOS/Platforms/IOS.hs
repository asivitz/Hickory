{-# LANGUAGE ForeignFunctionInterface, BlockArguments #-}
{-# LANGUAGE TupleSections, PatternSynonyms, OverloadedLists #-}

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
import Hickory.Vulkan.Vulkan (VulkanResources, Swapchain, unWrapAcquire)
import Hickory.Vulkan.Frame (FrameContext)
import Vulkan (MetalSurfaceCreateInfoEXT(..), CAMetalLayer, createMetalSurfaceEXT, pattern EXT_METAL_SURFACE_EXTENSION_NAME, Instance )
import Vulkan.Zero (Zero(..))
import Hickory.Vulkan.Utils (buildFrameFunction)
import Acquire.Acquire (Acquire)

foreign import ccall "getResourcePath" c'getResourcePath :: CString -> CInt -> IO ()

foreign export ccall "touch_began" touchBegan :: StablePtr (a,TouchData) -> CInt -> CDouble -> CDouble -> IO ()
foreign export ccall "touch_moved" touchMoved :: StablePtr (a,TouchData) -> CInt -> CDouble -> CDouble -> IO ()
foreign export ccall "touch_ended" touchEnded :: StablePtr (a,TouchData) -> CInt -> CDouble -> CDouble -> IO ()
foreign export ccall "draw"        draw       :: StablePtr (IO (),a) -> IO ()

type CDrawInit = Ptr CAMetalLayer -> CInt -> CInt -> IO (StablePtr (IO (), TouchData))

type RenderInit resources
  =  Size Int
  -> Instance
  -> VulkanResources
  -> Swapchain
  -> Acquire resources

type DrawInit resources gamedata
  =  CoreEventGenerators (resources, FrameContext)
  -> (gamedata -> IO ())
  -> IO gamedata
  -> IO ()

mkDrawInit
  :: RenderInit resources        -- Loads assets from a given folder and provides data for use in game/render loop
  -> DrawInit resources gamedata -- Initializes logic/draw system
  -> CDrawInit
mkDrawInit ri di layerPtr w h = do
  let mkSurface inst = createMetalSurfaceEXT inst zero { layer = layerPtr } Nothing

  td           <- initTouchData
  inputPoller  <- makeInputPoller (touchFunc td)
  timePoller   <- makeTimePoller
  wSizeRef     <- newIORef (Size (fromIntegral w) (fromIntegral h))

  (coreEvProc, evGens) <- coreEventGenerators inputPoller timePoller wSizeRef
  ((exeFrame, cleanupUserRes), cleanupVulkan)
    <- unWrapAcquire
     $ buildFrameFunction [EXT_METAL_SURFACE_EXTENSION_NAME] (pure $ Size (fromIntegral w) (fromIntegral h)) mkSurface ri
                          \userRes frameContext -> do
                            coreEvProc (userRes, frameContext)

  di evGens (error "Can't write state") (error "Can't load state")
  newStablePtr (exeFrame, td)

resourcesPath :: IO String
resourcesPath = do
  ptr <- mallocArray 1024
  c'getResourcePath ptr 1024
  str <- peekCString ptr
  free ptr
  return str

draw :: StablePtr (IO (), a) -> IO ()
draw pkg = do
  (drawF,_) <- deRefStablePtr pkg
  drawF
  performMinorGC

type TouchData =
  ( IORef [(Int, Scalar, Scalar, UTCTime)]
  , IORef [(Int, Scalar, Scalar, UTCTime)]
  , IORef [(Int, Scalar, Scalar)]
  , IORef (HashMap.HashMap Int (V2 Scalar, UTCTime))
  )

initTouchData :: IO TouchData
initTouchData = (,,,) <$> newIORef [] <*> newIORef [] <*> newIORef [] <*> newIORef HashMap.empty

touchBegan :: StablePtr (a, TouchData) -> CInt -> CDouble -> CDouble -> IO ()
touchBegan touchData ident x y = do
  (_, (newDowns, _, _, _)) <- deRefStablePtr touchData
  time <- getCurrentTime
  modifyIORef newDowns ((fromIntegral ident, realToFrac x, realToFrac y, time):)

touchMoved :: StablePtr (a, TouchData) -> CInt -> CDouble -> CDouble -> IO ()
touchMoved touchData ident x y = do
  (_, (_, _, newMoves, _)) <- deRefStablePtr touchData
  modifyIORef newMoves ((fromIntegral ident, realToFrac x, realToFrac y):)

touchEnded :: StablePtr (a, TouchData) -> CInt -> CDouble -> CDouble -> IO ()
touchEnded touchData ident x y = do
  (_, (_, newUps, _, _)) <- deRefStablePtr touchData
  time <- getCurrentTime
  modifyIORef newUps ((fromIntegral ident, realToFrac x, realToFrac y, time):)

touchFunc :: TouchData -> (RawInput -> IO ()) -> IO (IO ())
touchFunc touchData handleRawInput = pure $ do
  let (newDowns, newUps, newMoves, touches) = touchData

  curhash <- readIORef touches
  moves <- atomicModifyIORef newMoves ([],)
  ups <- atomicModifyIORef newUps ([],)
  downs <- atomicModifyIORef newDowns ([],)

  -- Update the touch positions for move events
  let hash' = foldl (\hsh (ident, x, y) -> HashMap.adjust (\(_, time) -> (V2 x y, time)) ident hsh) curhash moves

  -- Broadcast a loc event for touches held over from last frame
  handleRawInput (InputTouchesLoc (map (\(ident, (loc, _)) -> (loc, ident)) (HashMap.toList hash')))

  -- Add new touches
  handleRawInput (InputTouchesDown (map (\(ident, x, y, _time) -> (V2 x y, ident)) downs))
  let hash'' = foldl (\hsh (ident, x, y, time) -> HashMap.insert ident (V2 x y, time) hsh) hash' downs

  -- Remove released touches
  handleRawInput (InputTouchesUp (map (\(ident, x, y, time) ->
                                                          case HashMap.lookup ident hash'' of
                                                              Nothing -> (0, V2 x y, ident)
                                                              Just (_, prev) -> (realToFrac (diffUTCTime time prev), V2 x y, ident))
                                                      ups))
  let hash''' = foldl (\hsh (ident, _x, _y, _time) -> HashMap.delete ident hsh) hash'' ups

  -- Record the new hash
  writeIORef touches hash'''
