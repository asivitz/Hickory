module Platform.IPhone where

import Foreign.Marshal.Array
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc

foreign import ccall "getResourcePath" c'getResourcePath
    :: CString -> CInt -> IO ()

resourcesPath :: IO String
resourcesPath = do
        ptr <- mallocArray 1024
        c'getResourcePath ptr 1024
        str <- peekCString ptr
        free ptr
        return str

