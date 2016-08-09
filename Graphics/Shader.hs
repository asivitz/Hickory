module Graphics.Shader (
              Shader(..),
              nullShader,
              loadShader,
              loadShader',
              getShader
              )
              where

import Foreign.C.Types
import Foreign.C.String

newtype Shader = Shader CUInt deriving (Eq, Ord, Show)

getShader (Shader num) = num

nullShader :: Shader
nullShader = Shader (-1)

loadShaderFromPaths :: String -> String -> IO (Maybe Shader)
loadShaderFromPaths vertShaderPath fragShaderPath =
        withCString vertShaderPath $ \vpath ->
            withCString fragShaderPath $ \fpath ->
                do
                    val <- c'loadShader vpath fpath
                    if val >= 0
                        then
                            return $ Just $ Shader val
                        else return Nothing

loadShader resPath vert frag = do
   let prefix = resPath ++ "/Shaders/"
   let (vsp, fsp) = ( prefix ++ vert, prefix ++ frag)

   shader <- loadShaderFromPaths vsp fsp
   return shader

loadShader' resPath vert frag = do
        s <- loadShader resPath vert frag
        case s of
            Just sh -> return sh
            Nothing -> error $ "Can't load shader: " ++ vert ++ " " ++ frag

foreign import ccall "load_shader" c'loadShader
    :: CString -> CString -> IO CUInt
