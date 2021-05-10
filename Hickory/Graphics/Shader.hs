{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Hickory.Graphics.Shader
  ( loadShader,
    loadShaderFromPaths,
    useShader,
    getAttribLocation,
    UniformLoc,
    retrieveLoc,
    ShaderID,
    Shader (..),
  )
where

import Control.Monad
import qualified Data.HashMap.Strict as HashMap
import Data.Int (Int32)
import Data.Text (Text, unpack)
import qualified Data.Text.Foreign as FText
import Data.Word (Word32)
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL.Compatibility41 as GL
import Hickory.Utils.Utils

data Shader = Shader
  { program :: ProgramID,
    vertShader :: ShaderID,
    fragShader :: ShaderID,
    uniformLocs :: HashMap.HashMap String UniformLoc
  }
  deriving (Show)

type UniformLoc = Int32

type ShaderID = Word32

type ProgramID = Word32

retrieveLoc :: String -> Shader -> Maybe UniformLoc
retrieveLoc name shader = HashMap.lookup name (uniformLocs shader)

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
shaderSourceForPath = readFileAsText


-- TODO: iOS shouldn't have this header
{-return $ Text.append "#version 150\n" source-}

loadShaderFromPaths :: Text -> String -> String -> String -> [String] -> IO Shader
loadShaderFromPaths version resPath vert frag uniforms = do
  let prefix = resPath ++ "/Shaders/"
      vpath = prefix ++ vert
      fpath = prefix ++ frag
  vsource <- shaderSourceForPath vpath
  fsource <- shaderSourceForPath fpath

  loadShader version vsource fsource uniforms

loadShader :: Text -> Text -> Text -> [String] -> IO Shader
loadShader version vert frag uniforms = do
  let vsource = addVersion vert
      fsource = addVersion frag

  vs <- compileVertShader vsource
  fs <- compileFragShader fsource

  case vs of
    Just vsh -> case fs of
      Just fsh -> do
        prog <- buildShaderProgram vsh fsh uniforms
        case prog of
          Just pr -> return pr
          Nothing -> do
            glDeleteShader vsh
            glDeleteShader fsh
            error $ "Failed to link shader program: " ++ unpack vert ++ "\n\n" ++ unpack frag
      Nothing -> error $ "Couldn't load frag shader: " ++ unpack frag
    Nothing -> error $ "Couldn't load vertex shader: " ++ unpack vert
  where
    addVersion :: Text -> Text
    addVersion = mappend ("#version " <> version <> "\n")

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
      res <-
        sh
          <$> ( foldM
                  ( \hsh name -> do
                      loc <- getUniformLocation programId name
                      return $ HashMap.insert name loc hsh
                  )
                  HashMap.empty
                  uniforms
              )
      return $ Just res

useShader :: Shader -> IO ()
useShader Shader {program} = glUseProgram program
