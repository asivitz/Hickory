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
import Data.Maybe (catMaybes, fromMaybe)

data Shader = Shader
  { program :: ProgramID,
    shaderIds :: [ShaderID],
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

linkProgram :: GLuint -> [GLuint] -> IO Bool
linkProgram programId shaders = do
  mapM_ (glAttachShader programId) shaders
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

loadShaderFromPaths :: String -> String -> String -> [String] -> IO Shader
loadShaderFromPaths resPath vert frag uniforms = do
  let prefix = resPath ++ "/Shaders/"
      vpath = prefix ++ vert
      fpath = prefix ++ frag
  vsource <- shaderSourceForPath vpath
  fsource <- shaderSourceForPath fpath

  loadShader vsource Nothing fsource uniforms

loadShader :: Text -> Maybe Text -> Text -> [String] -> IO Shader
loadShader vert mgeom frag uniforms = do
  let shaders = catMaybes
        [ (,GL_GEOMETRY_SHADER) <$> mgeom
        , Just (vert, GL_VERTEX_SHADER)
        , Just (frag, GL_FRAGMENT_SHADER)
        ]
  shIds <- forM shaders \(source, typ) -> do
    fromMaybe (error $ "Couldn't compile shader: " ++ unpack source) <$>
      compileShader source typ

  prog <- buildShaderProgram shIds uniforms
  case prog of
    Just pr -> return pr
    Nothing -> do
      mapM_ glDeleteShader shIds
      error $ "Failed to link shader program: " ++ unpack vert ++ "\n\n" ++ maybe "" unpack mgeom ++ "\n\n" ++ unpack frag

buildShaderProgram :: [ShaderID] -> [String] -> IO (Maybe Shader)
buildShaderProgram shaders uniforms = do
  programId <- glCreateProgram

  linked <- linkProgram programId shaders
  if not linked
    then return Nothing
    else do
      let sh = Shader programId shaders
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
