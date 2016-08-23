{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NamedFieldPuns #-}

{-
module Graphics.GLSupport ( module Graphics.GL.ARB.ES2Compatibility ) where
import Graphics.GL.ARB.ES2Compatibility
-}

module GLInterface.GLSupport --( module Graphics.GL.Compatibility41)
    (
     pattern GL.GL_RGB,
     pattern GL.GL_RGBA,
     GL.glDeleteShader,

     genTexture,
     bindVAO,
     Shader(..),
     useShader,
     runGL,
     bufferVertices,
     bufferIndices,
     loadGLTex,
     createVAOConfig,
     indexVAOConfig,
     drawCommand,
     buildShaderProgram,
     compileVertShader,
     compileFragShader,
     configGLState,
     clearScreen
     )
    where

import Graphics.GL.Compatibility41 as GL
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Utils
import Foreign.Marshal.Array
import Control.Monad.Reader
import Types.Color
import Math.Matrix
import qualified Data.Foldable as Fold
import Math.VectorMatrix
import Data.Bits
import Graphics.Drawing

-- Unused?
type GLMonad c o = ReaderT c IO o
runGL context mon = runReaderT mon context

genTexture = alloca $ \p ->
    do
        GL.glGenTextures 1 p
        peek p

withNewPtr :: Storable b => (Ptr b -> IO a) -> IO b
withNewPtr f = alloca (\p -> f p >> peek p)

makeVAO :: IO VAO
makeVAO = withNewPtr (GL.glGenVertexArrays 1)

makeVBO :: IO VAO
makeVBO = withNewPtr (GL.glGenBuffers 1)

bindVAO :: VAO -> IO ()
bindVAO = GL.glBindVertexArray

bufferVertices :: VBO -> [CFloat] -> IO ()
bufferVertices vbo floats = do
        GL.glBindBuffer GL.GL_ARRAY_BUFFER vbo
        withArrayLen floats $ \len ptr ->
            GL.glBufferData GL.GL_ARRAY_BUFFER
                         (fromIntegral (len * sizeOf (0::GL.GLfloat)))
                         ptr
                         GL.GL_STREAM_DRAW
        _ <- GL.glUnmapBuffer GL.GL_ARRAY_BUFFER
        return ()

bufferIndices :: VBO -> [CUShort] -> IO ()
bufferIndices vbo ints = do
        GL.glBindBuffer GL.GL_ELEMENT_ARRAY_BUFFER vbo

        withArrayLen ints $ \len ptr ->
            GL.glBufferData GL.GL_ELEMENT_ARRAY_BUFFER
                        (fromIntegral (len * sizeOf (0::GL.GLushort)))
                        ptr
                        GL.GL_STREAM_DRAW

        _ <- GL.glUnmapBuffer GL.GL_ELEMENT_ARRAY_BUFFER
        return ()

-- Shaders
useShader (Shader { program }) = glUseProgram program

-- VAO / VBO

glenumForDrawType dt = case dt of
                           TriangleStrip -> GL.GL_TRIANGLE_STRIP
                           TriangleFan -> GL.GL_TRIANGLE_FAN
                           Triangles -> GL.GL_TRIANGLES

drawCommand :: Shader -> Mat44 -> Color -> Color -> Maybe TexID -> VAO -> GLint -> DrawType -> IO ()
drawCommand shader mat color color2 texid vao numitems drawType = do
        useShader shader
        case texid of
            Just t -> glBindTexture GL_TEXTURE_2D (fromIntegral $ getTexID t)
            _ -> return ()

        withVec4 color $ \ptr ->
            glUniform4fv (sp_UNIFORM_COLOR shader) 1 (castPtr ptr)
        withVec4 color2 $ \ptr ->
            glUniform4fv (sp_UNIFORM_COLOR2 shader) 1 (castPtr ptr)

        withMat44 mat $ \ptr ->
            glUniformMatrix4fv (sp_UNIFORM_MODEL_MAT shader) 1 GL_FALSE (castPtr ptr)
        withMat44 identity $ \ptr ->
            glUniformMatrix4fv (sp_UNIFORM_VIEW_MAT shader) 1 GL_FALSE (castPtr ptr)
        glBindVertexArray vao

        glDrawElements (glenumForDrawType drawType) numitems GL_UNSIGNED_SHORT nullPtr

attachVertexArray :: GLuint -> GLint -> GLint -> GLint -> IO ()
attachVertexArray attrLoc len stride offset = do
        glEnableVertexAttribArray attrLoc
        glVertexAttribPointer attrLoc len GL_FLOAT GL_FALSE (stride * fsize) (plusPtr nullPtr (fromIntegral $ offset * fsize))
    where fsize = fromIntegral $ sizeOf (0 :: GLfloat)

buildVertexGroup :: Shader -> VertexGroup -> IO VBO
buildVertexGroup shader (VertexGroup attachments) = do
        vbo <- makeVBO
        glBindBuffer GL_ARRAY_BUFFER vbo

        let stride = sum $ map (\(Attachment a l) -> l) attachments

        Fold.foldlM (\offset (Attachment a l) -> do
                attachVertexArray (a shader) l stride offset
                return (offset + l))
            0
            attachments

        return vbo

indexVAOConfig :: VAOConfig -> IO VAOConfig
indexVAOConfig config@VAOConfig { vao } = do
        glBindVertexArray vao
        index_vbo <- makeVBO
        glBindBuffer GL_ELEMENT_ARRAY_BUFFER index_vbo

        return $ config { indexVBO = Just index_vbo }

createVAOConfig :: Shader -> [VertexGroup] -> IO VAOConfig
createVAOConfig sh vertexgroups = do
        vao' <- makeVAO
        glBindVertexArray vao'

        buffers <- mapM (buildVertexGroup sh) vertexgroups

        return VAOConfig { vao = vao',
                         indexVBO = Nothing,
                         vertices = buffers
                         }

-- Textures

-- create linear filtered texture
loadGLTex format w h ptr = do
    tex <- genTexture
    GL.glBindTexture GL.GL_TEXTURE_2D tex

    GL.glTexImage2D GL.GL_TEXTURE_2D 0 (fromIntegral format)
        (fromIntegral w) (fromIntegral h)
        0 format GL.GL_UNSIGNED_BYTE ptr

    GL.glGenerateMipmap GL.GL_TEXTURE_2D

    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MAG_FILTER (fromIntegral GL.GL_LINEAR)
    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER (fromIntegral GL.GL_LINEAR_MIPMAP_LINEAR)
    return $ Just $ TexID (fromIntegral tex)

-- Shaders

glGetBoolean boolean = (== GL_TRUE) <$> alloca (\ptr -> glGetBooleanv boolean ptr >> peek ptr)

glGetShaderi shaderId x = alloca (\ptr -> glGetShaderiv shaderId x ptr >> peek ptr)

glGetProgrami programId x = alloca (\ptr -> glGetProgramiv programId x ptr >> peek ptr)

retrieveLog lenFun infoFun = do
        infoLen <- lenFun GL_INFO_LOG_LENGTH
        let infoLen' = fromIntegral infoLen
        if (infoLen > 1)
            then do
                info <- allocaArray infoLen' $ \buf -> do
                    infoFun infoLen nullPtr buf
                    peekCStringLen (buf, infoLen' - 1)
                return $ Just info
            else return Nothing


compileShader :: String -> GLenum -> IO (Maybe GLuint)
compileShader source shaderType = do
        compileSupported <- glGetBoolean GL_SHADER_COMPILER
        unless compileSupported (error "ERROR: Shader compilation not supported.")

        shaderId <- glCreateShader shaderType

        withMany withCString [source] $ \strs ->
            withArray strs $ \ptr ->
                glShaderSource shaderId 1 (castPtr ptr) nullPtr
        glCompileShader shaderId
        compiled <- glGetShaderi shaderId GL_COMPILE_STATUS
        let didCompile = compiled /= 0

        if didCompile
            then return $ Just shaderId
            else do
                infoLog <- retrieveLog (glGetShaderi shaderId) (glGetShaderInfoLog shaderId)
                case infoLog of
                    Just i -> do
                        print "*** ERROR: Couldn't compile shader"
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
                    print "*** ERROR: Can't link shader program"
                    print i
                _ -> return ()

        return didLink

compileVertShader :: String -> IO (Maybe GLuint)
compileVertShader source = compileShader source GL_VERTEX_SHADER

compileFragShader :: String -> IO (Maybe GLuint)
compileFragShader source = compileShader source GL_FRAGMENT_SHADER

buildShaderProgram :: GLuint -> GLuint -> IO (Maybe Shader)
buildShaderProgram vertShader fragShader = do
        programId <- glCreateProgram

        linked <- linkProgram programId vertShader fragShader
        if not linked
            then return Nothing
            else do
                let sh = Shader programId vertShader fragShader
                res <- sh <$>
                    (fromIntegral <$> withCString "position" (glGetAttribLocation programId)) <*>
                    (fromIntegral <$> withCString "texCoords" (glGetAttribLocation programId)) <*>
                    (fromIntegral <$> withCString "color" (glGetAttribLocation programId)) <*>
                    (fromIntegral <$> withCString "color2" (glGetAttribLocation programId)) <*>
                    (fromIntegral <$> withCString "normals" (glGetAttribLocation programId)) <*>

                    (withCString "tex" (glGetUniformLocation programId)) <*>
                    (withCString "color" (glGetUniformLocation programId)) <*>
                    (withCString "color2" (glGetUniformLocation programId)) <*>
                    (withCString "modelMat" (glGetUniformLocation programId)) <*>
                    (withCString "viewMat" (glGetUniformLocation programId)) <*>
                    (withCString "size" (glGetUniformLocation programId))
                return $ Just res

configGLState :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> m ()
configGLState r g b = do
        glClearColor r g b 1
        glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        glActiveTexture GL_TEXTURE0

        glDisable GL_DITHER
        glDisable GL_STENCIL_TEST

        glEnable GL_PROGRAM_POINT_SIZE -- for OSX

        glEnable GL_BLEND

clearScreen :: MonadIO m => m ()
clearScreen = do
        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
