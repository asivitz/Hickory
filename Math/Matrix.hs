module Math.Matrix where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.ForeignPtr
import System.IO.Unsafe

newtype Mat44 = Mat44 (ForeignPtr CFloat)
type Mat44Raw = (Ptr CFloat)
{-newtype Mat44 = Mat44 (Ptr CFloat)-}

instance Show Mat44 where
      show m = show $ mat44ToList m

withMat44 :: Mat44 -> (Ptr CFloat -> IO b) -> IO b
withMat44 (Mat44 fptr) func =
      withForeignPtr fptr $ \ptr ->
         func ptr

buildMat44 :: (Mat44Raw -> IO ()) -> IO Mat44
buildMat44 f =
   do fptr <- mallocForeignPtrArray 16
      withForeignPtr fptr $ \ptr -> f ptr
      return $ Mat44 fptr

mat44Identity :: Mat44
mat44Identity = unsafePerformIO $ buildMat44 c'mat44Identity

mat44Mul :: Mat44 -> Mat44 -> Mat44
mat44Mul a b = unsafePerformIO $ buildMat44 $ \m ->
   withMat44 a $ \ap ->
      withMat44 b $ \bp ->
         c'mat44Mul m ap bp

mat44Ortho :: Real a => a -> a -> a -> a -> a -> a -> Mat44
mat44Ortho l r b t n f = unsafePerformIO $ buildMat44 $ \m -> c'mat44Ortho m (realToFrac l) (realToFrac r) 
    (realToFrac b) (realToFrac t) 
    (realToFrac n) (realToFrac f)

mat44Perspective :: Real a => a -> a -> a -> a -> Mat44
mat44Perspective fov aspect near far = 
        unsafePerformIO $
            buildMat44 $ \m ->
                c'mat44Perspective m (realToFrac fov) (realToFrac aspect) (realToFrac near) (realToFrac far)

foreign import ccall "mat4x4_perspective" c'mat44Perspective
    :: Mat44Raw -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()

mat44ToList :: Mat44 -> [Float]
mat44ToList m = unsafePerformIO $ 
                  withMat44 m $ \ptr ->
                     do
                        lst <- peekArray 16 ptr
                        return $ fmap realToFrac lst

mat44Scale :: Float -> Float -> Float -> Mat44 -> Mat44
mat44Scale x y z a = unsafePerformIO $ buildMat44 $ \m ->
   withMat44 a $ \ap -> 
      c'mat44Scale m ap (realToFrac x) (realToFrac y) (realToFrac z)

mat44Translate :: Float -> Float -> Float -> Mat44 -> Mat44
mat44Translate x y z a = mat44Mul a $ 
   unsafePerformIO $ buildMat44 $ \m ->
      c'mat44Translate m (realToFrac x) (realToFrac y) (realToFrac z)

mat44Invert :: Mat44 -> Mat44
mat44Invert m = unsafePerformIO $ buildMat44 $ \p ->
    withMat44 m $ \mp ->
        c'mat44Invert p mp

foreign import ccall "mat4x4_invert" c'mat44Invert
    :: Mat44Raw -> Mat44Raw -> IO ()

foreign import ccall "mat4x4_identity" c'mat44Identity
   :: Mat44Raw -> IO ()

foreign import ccall "mat4x4_mul" c'mat44Mul
   :: Mat44Raw -> Mat44Raw -> Mat44Raw -> IO ()

foreign import ccall "mat4x4_ortho" c'mat44Ortho
   :: Mat44Raw -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()

foreign import ccall "mat4x4_scale_aniso" c'mat44Scale
   :: Mat44Raw -> Mat44Raw -> CFloat -> CFloat -> CFloat -> IO ()

foreign import ccall "mat4x4_translate" c'mat44Translate
   :: Mat44Raw -> CFloat -> CFloat -> CFloat -> IO ()
