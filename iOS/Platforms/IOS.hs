{-# LANGUAGE ForeignFunctionInterface #-}

module Platforms.IOS where

import Graphics.GL.Compatibility41 as GL
import Data.Bits
import Hickory.Graphics.Drawing
{-import Engine.Scene.Scene-}
{-import Engine.Scene.Run-}
import Hickory.Types
import Data.Time
import Data.IORef
import Hickory.Input
import Hickory.Math.Vector
import Hickory.Platform
import qualified Data.HashMap.Strict as HashMap

import Foreign
import Foreign.C

import Foreign.Marshal.Array
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc

{-foreign import ccall safe "register_touch_began_callback" regTouchBeganCallback :: FunPtr (CInt -> CDouble -> CDouble -> IO ()) -> IO ()-}
{-foreign import ccall safe "register_touch_ended_callback" regTouchEndedCallback :: FunPtr (CInt -> CDouble -> CDouble -> IO ()) -> IO ()-}
{-foreign import ccall safe "register_touch_moved_callback" regTouchMovedCallback :: FunPtr (CInt -> CDouble -> CDouble -> IO ()) -> IO ()-}
{-foreign import ccall safe "register_init_draw_callback" regInitDrawCallback :: FunPtr (CInt -> CInt -> IO ()) -> IO ()-}
{-foreign import ccall safe "register_draw_frame_callback" regDrawFrameCallback :: FunPtr (IO ()) -> IO ()-}
{-foreign import ccall "wrapper" mkTouchFunWrap :: (CInt -> CDouble -> CDouble -> IO ()) -> IO (FunPtr (CInt -> CDouble -> CDouble -> IO ()))-}
{-foreign import ccall "wrapper" mkDrawFrame :: IO () -> IO (FunPtr (IO ()))-}
{-foreign import ccall "wrapper" mkInit :: (CInt -> CInt -> IO ()) -> IO (FunPtr (CInt -> CInt -> IO ()))-}
{-foreign import ccall safe "c_main" c_main :: IO ()-}
foreign import ccall "getResourcePath" c'getResourcePath :: CString -> CInt -> IO ()

foreign export ccall "touch_began" touchBegan :: StablePtr (a,TouchData) -> CInt -> CDouble -> CDouble -> IO ()
foreign export ccall "touch_moved" touchMoved :: StablePtr (a,TouchData) -> CInt -> CDouble -> CDouble -> IO ()
foreign export ccall "touch_ended" touchEnded :: StablePtr (a,TouchData) -> CInt -> CDouble -> CDouble -> IO ()

resourcesPath :: IO String
resourcesPath = do
        ptr <- mallocArray 1024
        c'getResourcePath ptr 1024
        str <- peekCString ptr
        free ptr
        return str

{-mkTouchFun f = mkTouchFunWrap (\ident x y -> f (fromIntegral ident) (realToFrac x) (realToFrac y))-}

{-
makeIOSStepFunc :: Show ie => [SceneOperator ie] -> IO () -> IO (IO ())
makeIOSStepFunc operators stepInput = do
        start_time <- getCurrentTime
        timeref <- newIORef start_time

        return $ do
            prev_time <- readIORef timeref
            current_time <- getCurrentTime
            let delta = min 0.1 $ realToFrac (diffUTCTime current_time prev_time)
            writeIORef timeref current_time

            stepInput
            runOneFrame operators iosRender delta

iosInitFunc :: [SceneOperator ie] -> CInt -> CInt -> IO ()
iosInitFunc ops w h = iosInit (Size (fromIntegral w) (fromIntegral h)) ops
-}

{-makeIOSInputPoller :: IO (IO [RawInput])-}
{-makeIOSInputPoller = makeInputPoller iosInputInit-}

type TouchData = (IORef [(Int, Double, Double, UTCTime)], IORef [(Int, Double, Double, UTCTime)], IORef [(Int, Double, Double)], IORef (HashMap.HashMap Int (V2 Scalar, UTCTime)))

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
  moves <- atomicModifyIORef newMoves (\a -> ([], a))
  ups <- atomicModifyIORef newUps (\a -> ([], a))
  downs <- atomicModifyIORef newDowns (\a -> ([], a))

  -- Update the touch positions for move events
  let hash' = foldl (\hsh (ident, x, y) -> HashMap.adjust (\(_, time) -> (v2 x y, time)) ident hsh) curhash moves

  -- Broadcast a loc event for touches held over from last frame
  handleRawInput (InputTouchesLoc (map (\(ident, (loc, _)) -> (loc, ident)) (HashMap.toList hash')))

  -- Add new touches
  handleRawInput (InputTouchesDown (map (\(ident, x, y, time) -> (v2 x y, ident)) downs))
  let hash'' = foldl (\hsh (ident, x, y, time) -> HashMap.insert ident (v2 x y, time) hsh) hash' downs

  -- Remove released touches
  handleRawInput (InputTouchesUp (map (\(ident, x, y, time) ->
                                                          case HashMap.lookup ident hash'' of
                                                              Nothing -> (0, v2 x y, ident)
                                                              Just (_, prev) -> (realToFrac (diffUTCTime time prev), v2 x y, ident))
                                                      ups))
  let hash''' = foldl (\hsh (ident, x, y, time) -> HashMap.delete ident hsh) hash'' ups

  -- Record the new hash
  writeIORef touches hash'''

{-
iosInit :: Size Int -> [SceneOperator ie] -> IO ()
iosInit scrSize operators = do
        initRenderer
        glClearColor 0.125 0.125 0.125 1
        glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA
        glActiveTexture gl_TEXTURE0

        {-glEnable gl_PROGRAM_POINT_SIZE -- for OSX-}

        mapM_ (\op -> (_initRenderer op) scrSize) operators

iosRender :: [SceneOperator ie] -> IO ()
iosRender operators = do
        glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)

        mapM_ _renderOp operators

        resetRenderer
        -}

mapAll :: [a -> b] -> a -> [b]
mapAll fs a = map (\f -> f a) fs

{-
iosMain :: Show ie => [SceneOperator ie] -> (RawInput -> IO ()) -> IO ()
iosMain operators inputHandler = do
        inputStep <- iosInputInit inputHandler

        drawFrame <- mkDrawFrame =<< (makeIOSStepFunc operators inputStep)

        initCallback <- mkInit $ iosInitFunc operators

        regInitDrawCallback initCallback
        regDrawFrameCallback drawFrame

        c_main
        -}
