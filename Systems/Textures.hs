{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module Systems.Textures (SysData(..), empty, make, releaseTex) where

import Engine.System
import Engine.World
import Graphics.GLUtils
import Control.Monad.State
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Data.IORef
import Data.Array.Storable
import Graphics.Rendering.OpenGL.Raw.Core31

import Utils.Resources

import Codec.Image.PNG

data SysData = SysData { 
             textures :: RefStore String TexID
             }
             deriving (Show)

empty :: SysData
empty = SysData { textures = emptyRefStore }

make textures = System run (initS textures)

initS textures = do
        registerResource reserveTex (reserveTex' textures)
        registerResource releaseTex (releaseTex' textures)

loadTexture :: String -> IO (Maybe TexID)
loadTexture path = do
        res <- loadPNGFile path
        case res of
            Left s -> print s >> return Nothing
            Right image -> do
                let (w, h) = dimensions image
                withStorableArray (imageData image) $ \pd -> do
                    tex <- alloca $ \p -> do
                                glGenTextures 1 p
                                peek p
                    let glLinear  = fromIntegral gl_LINEAR
                        format = if hasAlphaChannel image then gl_RGBA else gl_RGB
                    -- create linear filtered texture
                    glBindTexture gl_TEXTURE_2D tex

                    glTexImage2D gl_TEXTURE_2D 0 (fromIntegral format)
                        (fromIntegral w) (fromIntegral h)
                        0 format gl_UNSIGNED_BYTE pd

                    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER glLinear
                    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER glLinear
                    return $ Just $ TexID (fromIntegral tex)

foreign import ccall "getResourcePath" c'getResourcePath
    :: CString -> CInt -> IO ()

resourcePath :: IO String
resourcePath = do
        ptr <- mallocArray 1024
        c'getResourcePath ptr 1024
        str <- peekCString ptr
        free ptr
        return str

deleteTexture :: TexID -> IO ()
deleteTexture texid = return ()

reserveTex' :: IORef SysData -> String -> SysMonad c IO (Maybe TexID)
reserveTex' texes path = do
   mydata@SysData { textures } <- getSysData texes
   (newtexes, texid) <- liftIO $ reserve textures path $ \p -> do 
                                                                  rp <- resourcePath
                                                                  loadTexture $ rp ++ "/images/" ++ p
   putSysData texes mydata { textures = newtexes }
   return texid

releaseTex' :: IORef SysData -> String -> SysMonad c IO ()
releaseTex' texes path = do
   mydata@SysData { textures } <- getSysData texes
   newtexes <- liftIO $ release textures path deleteTexture
   putSysData texes mydata { textures = newtexes }

run delta = 
   do
      return ()
