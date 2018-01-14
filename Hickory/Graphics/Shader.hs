{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Hickory.Graphics.Shader (
              loadShader,
              useShader,
              deleteShader
              )
              where

import Control.Monad
import Data.Semigroup
import Data.Text (Text)
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Hickory.Graphics.Drawing
import Hickory.Utils.Utils
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.Foreign as FText

#if defined(ghcjs_HOST_OS)
import Graphics.GL.Compatibility41 as GL (GLenum, GLfloat, GLint, GLuint, GLushort, GLboolean, GLsizei, GLintptr,
                                         pattern GL_SRC_ALPHA,
                                         pattern GL_ONE_MINUS_SRC_ALPHA,
                                         pattern GL_TEXTURE0,
                                         pattern GL_DITHER,
                                         pattern GL_STENCIL_TEST,
                                         pattern GL_PROGRAM_POINT_SIZE,
                                         pattern GL_BLEND,
                                         pattern GL_COLOR_BUFFER_BIT,
                                         pattern GL_DEPTH_BUFFER_BIT,
                                         pattern GL_FRAGMENT_SHADER,
                                         pattern GL_VERTEX_SHADER,
                                         pattern GL_LINK_STATUS,
                                         pattern GL_COMPILE_STATUS,
                                         pattern GL_SHADER_COMPILER,
                                         pattern GL_INFO_LOG_LENGTH,
                                         pattern GL_LINEAR_MIPMAP_LINEAR,
                                         pattern GL_TRUE,
                                         pattern GL_TEXTURE_MIN_FILTER,
                                         pattern GL_TEXTURE_2D,
                                         pattern GL_LINEAR,
                                         pattern GL_TEXTURE_MAG_FILTER,
                                         pattern GL_UNSIGNED_BYTE,
                                         pattern GL_ELEMENT_ARRAY_BUFFER,
                                         pattern GL_ARRAY_BUFFER,
                                         pattern GL_FALSE,
                                         pattern GL_FLOAT,
                                         pattern GL_UNSIGNED_SHORT,
                                         pattern GL_TRIANGLES,
                                         pattern GL_TRIANGLE_FAN,
                                         pattern GL_TRIANGLE_STRIP,
                                         pattern GL_STREAM_DRAW)

import Data.JSString (unpack, pack, JSString)
import Hickory.Utils.Utils

foreign import javascript safe "gl.deleteShader($1)" deleteShader :: ShaderID -> IO ()

foreign import javascript safe " \
    var shader = gl.createShader($2); \
    gl.shaderSource(shader, $1); \
    gl.compileShader(shader); \
    \
    if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) { \
        alert(gl.getShaderInfoLog(shader)); \
    } \
    $r = shader; \
    " compileShader' :: JSString -> GLenum -> IO ShaderID

compileShader :: Text.Text -> GLenum -> IO (Maybe ShaderID)
compileShader a b = compileShader' (pack . Text.unpack $ a) b >>= return . Just

foreign import javascript safe " \
    gl.attachShader($1, $2); \
    gl.attachShader($1, $3); \
    gl.linkProgram($1); \
    if (!gl.getProgramParameter($1, gl.LINK_STATUS)) { \
        alert(gl.getProgramInfoLog($1)); \
    } \
    $r = true; \
    " linkProgram :: ProgramID -> ShaderID -> ShaderID -> IO Bool

foreign import javascript safe "$r = gl.getUniformLocation($1,$2);" glGetUniformLocation :: ProgramID -> JSString -> IO UniformLoc
getUniformLocation :: ProgramID -> String -> IO UniformLoc
getUniformLocation a b = glGetUniformLocation a (pack b)

foreign import javascript safe "$r = gl.getAttribLocation($1,$2);" glGetAttribLocation :: ProgramID -> JSString -> IO GLint
getAttribLocation :: ProgramID -> String -> IO GLint
getAttribLocation a b = glGetAttribLocation a (pack b)

foreign import javascript safe "gl.useProgram($1)" glUseProgram :: ProgramID -> IO ()
foreign import javascript safe "$r = gl.createProgram();" glCreateProgram :: IO ProgramID

shaderSourceForPath path = readFileAsText path

#else
import Graphics.GL.Compatibility41 as GL

deleteShader = glDeleteShader
glGetShaderi shaderId x = alloca (\ptr -> glGetShaderiv shaderId x ptr >> peek ptr)

glGetProgrami programId x = alloca (\ptr -> glGetProgramiv programId x ptr >> peek ptr)

glGetBoolean boolean = (== GL_TRUE) <$> alloca (\ptr -> glGetBooleanv boolean ptr >> peek ptr)

retrieveLog :: (GLenum -> IO GLint) -> (GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()) -> IO (Maybe String)
retrieveLog lenFun infoFun = do
        infoLen <- lenFun GL_INFO_LOG_LENGTH
        let infoLen' = fromIntegral infoLen
        if infoLen > 1
           then do
               info <- allocaArray infoLen' $ \buf -> do
                   infoFun infoLen nullPtr buf
                   peekCStringLen (buf, infoLen' - 1)
               return $ Just info
           else return Nothing

compileShader :: Text -> GLenum -> IO (Maybe GLuint)
compileShader source shaderType = do
        compileSupported <- glGetBoolean GL_SHADER_COMPILER
        unless compileSupported (error "ERROR: Shader compilation not supported.")

        shaderId <- glCreateShader shaderType

        withMany FText.withCStringLen [source] $ \strLens -> do
            let (strs, lengths) = unzip strLens
            withArray strs $ \strsArr ->
                withArray (map fromIntegral lengths) $ \lenArr ->
                    glShaderSource shaderId 1 strsArr lenArr

        glCompileShader shaderId
        compiled <- glGetShaderi shaderId GL_COMPILE_STATUS

        let didCompile = compiled /= 0

        if didCompile
            then return $ Just shaderId
            else do
                infoLog <- retrieveLog (glGetShaderi shaderId) (glGetShaderInfoLog shaderId)
                case infoLog of
                    Just i -> do
                        print ("*** ERROR: Couldn't compile shader" :: String)
                        print i
                    _ -> return ()

                glDeleteShader shaderId
                return Nothing

linkProgram :: GLuint -> GLuint -> GLuint -> IO Bool
linkProgram programId vertShader fragShader = do
        glAttachShader programId vertShader
        glAttachShader programId fragShader
        glLinkProgram programId

        linked <- glGetProgrami programId GL_LINK_STATUS
        let didLink = linked /= 0

        unless didLink $ do
            infoLog <- retrieveLog (glGetProgrami programId) (glGetProgramInfoLog programId)
            case infoLog of
                Just i -> do
                    print ("*** ERROR: Can't link shader program" :: String)
                    print i
                _ -> return ()

        return didLink

getAttribLocation progId name = withCString name (glGetAttribLocation progId)
getUniformLocation progId name = withCString name (glGetUniformLocation progId)

shaderSourceForPath :: String -> IO Text
shaderSourceForPath path = do
        source <- readFileAsText path
        return source
        -- TODO: iOS shouldn't have this header
        {-return $ Text.append "#version 150\n" source-}

#endif

loadShader :: Text -> String -> String -> String -> [String] -> IO Shader
loadShader version resPath vert frag uniforms = do
  let prefix = resPath ++ "/Shaders/"
      vpath  = prefix ++ vert
      fpath  = prefix ++ frag
  vsource <- getSource vpath
  fsource <- getSource fpath

  vs      <- compileVertShader vsource
  fs      <- compileFragShader fsource

  case vs of
    Just vsh -> case fs of
      Just fsh -> do
        prog <- buildShaderProgram vsh fsh uniforms
        case prog of
          Just pr -> return pr
          Nothing -> do
            deleteShader vsh
            deleteShader fsh
            error $ "Failed to link shader program: " ++ vpath ++ "-" ++ fpath
      Nothing -> error $ "Couldn't load frag shader: " ++ fpath
    Nothing -> error $ "Couldn't load vertex shader: " ++ vpath
  where
    getSource :: String -> IO Text
    getSource pth = (("#version " <> version <> "\n") <>) <$> shaderSourceForPath pth

compileVertShader :: Text -> IO (Maybe ShaderID)
compileVertShader source = compileShader source GL_VERTEX_SHADER

compileFragShader :: Text -> IO (Maybe ShaderID)
compileFragShader source = compileShader source GL_FRAGMENT_SHADER

buildShaderProgram :: ShaderID -> ShaderID -> [String] -> IO (Maybe Shader)
buildShaderProgram vertShader fragShader uniforms = do
        programId <- glCreateProgram

        linked <- linkProgram programId vertShader fragShader
        if not linked
            then return Nothing
            else do
                let sh = Shader programId vertShader fragShader
                res <- sh <$>
                    (fromIntegral <$> getAttribLocation programId "position") <*>
                    (fromIntegral <$> getAttribLocation programId "texCoords") <*>
                    (fromIntegral <$> getAttribLocation programId "color") <*>
                    (fromIntegral <$> getAttribLocation programId "color2") <*>
                    (fromIntegral <$> getAttribLocation programId "normal") <*>
                    (fromIntegral <$> getAttribLocation programId "boneIndex") <*>
                    (fromIntegral <$> getAttribLocation programId "materialIndex") <*>
                    (foldM (\hsh name -> do
                        loc <- getUniformLocation programId name
                        return $ HashMap.insert name loc hsh) HashMap.empty uniforms)
                return $ Just res

useShader (Shader { program }) = glUseProgram program
