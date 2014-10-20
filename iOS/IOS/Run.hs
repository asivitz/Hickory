{-# LANGUAGE ForeignFunctionInterface #-}

module IOS.Run where

import Graphics.Rendering.OpenGL.Raw.Core31
import Data.Bits
import Graphics.Drawing
import Engine.Scene.Scene
import Engine.Scene.Run
import Types.Types
import Data.Time
import Data.IORef
import Engine.Scene.Input
import Math.Vector
import qualified Data.HashMap.Strict as HashMap

import Foreign
import Foreign.C
import Foreign.Ptr

foreign import ccall safe "register_touch_began_callback" regTouchBeganCallback :: FunPtr (CDouble -> CDouble -> IO ()) -> IO ()
foreign import ccall safe "register_touch_ended_callback" regTouchEndedCallback :: FunPtr (CDouble -> CDouble -> IO ()) -> IO ()
foreign import ccall safe "register_touch_moved_callback" regTouchMovedCallback :: FunPtr (CDouble -> CDouble -> IO ()) -> IO ()
foreign import ccall "wrapper" mkTouchFun :: (CDouble -> CDouble -> IO ()) -> IO (FunPtr (CDouble -> CDouble -> IO ()))

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

iosInputInit :: SceneOperator ie -> (RawInput Int -> ie) -> IO (IO ())
iosInputInit keyOperator pkgRawInput = do
        touches <- newIORef HashMap.empty :: IO (IORef (HashMap.HashMap Int (V2, UTCTime)))
        let td = (\x y -> do let loc = (v2 (realToFrac x) (realToFrac y))
                             (_addEvent keyOperator (pkgRawInput (InputTouchDown loc 0)))
                             time <- getCurrentTime
                             curhash <- readIORef touches
                             writeIORef touches $ HashMap.insert 0 (loc, time) curhash
                             )
            tu = (\x y -> do 
                    let loc = (v2 (realToFrac x) (realToFrac y))
                    curhash <- readIORef touches
                    case HashMap.lookup 0 curhash of
                        Nothing -> (_addEvent keyOperator (pkgRawInput (InputTouchUp 0.0 loc 0)))
                        Just (oldloc, prev) -> do
                            time <- getCurrentTime
                            let delta = realToFrac (diffUTCTime time prev)
                            writeIORef touches $ HashMap.delete 0 curhash
                            (_addEvent keyOperator (pkgRawInput (InputTouchUp delta loc 0)))
                )
            tm = (\x y -> do 
                    let loc = (v2 (realToFrac x) (realToFrac y))
                    curhash <- readIORef touches
                    writeIORef touches $ HashMap.adjust (\(_, time) -> (loc, time)) 0 curhash)

        regTouchBeganCallback =<< mkTouchFun td
        regTouchEndedCallback =<< mkTouchFun tu
        regTouchMovedCallback =<< mkTouchFun tm
        return $ do
            curhash <- readIORef touches
            case HashMap.lookup 0 curhash of
                Nothing -> return ()
                Just (loc, _) -> (_addEvent keyOperator (pkgRawInput (InputTouchLoc loc 0)))

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

mapAll :: [a -> b] -> a -> [b]
mapAll fs a = map (\f -> f a) fs
