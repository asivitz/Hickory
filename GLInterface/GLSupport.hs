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

#if defined(ghcjs_HOST_OS)
import Data.Word
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign
import Data.JSString (pack, JSString)
import Graphics.GL.Compatibility41 as GL (GLenum, GLfloat, GLint, GLuint, GLushort,
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
#else
import Graphics.GL.Compatibility41 as GL
#endif
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

withNewPtr :: Storable b => (Ptr b -> IO a) -> IO b
withNewPtr f = alloca (\p -> f p >> peek p)

bufferVertices :: VBO -> [CFloat] -> IO ()
bufferVertices vbo floats = do
        glBindBuffer GL_ARRAY_BUFFER vbo
        bufferData GL_ARRAY_BUFFER floats GL_STREAM_DRAW
        _ <- glUnmapBuffer GL_ARRAY_BUFFER
        return ()

bufferIndices :: VBO -> [CUShort] -> IO ()
bufferIndices vbo ints = do
        glBindBuffer GL_ELEMENT_ARRAY_BUFFER vbo
        bufferData GL_ELEMENT_ARRAY_BUFFER ints GL_STREAM_DRAW
        _ <- glUnmapBuffer GL_ELEMENT_ARRAY_BUFFER
        return ()

-- Shaders
useShader (Shader { program }) = glUseProgram program

-- VAO / VBO

glenumForDrawType dt = case dt of
                           TriangleStrip -> GL_TRIANGLE_STRIP
                           TriangleFan -> GL_TRIANGLE_FAN
                           Triangles -> GL_TRIANGLES

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
                    (fromIntegral <$> getAttribLocation programId "position") <*>
                    (fromIntegral <$> getAttribLocation programId "texCoords") <*>
                    (fromIntegral <$> getAttribLocation programId "color") <*>
                    (fromIntegral <$> getAttribLocation programId "color2") <*>
                    (fromIntegral <$> getAttribLocation programId "normals") <*>

                    getUniformLocation programId "tex" <*>
                    getUniformLocation programId "color" <*>
                    getUniformLocation programId "color2" <*>
                    getUniformLocation programId "modelMat" <*>
                    getUniformLocation programId "viewMat" <*>
                    getUniformLocation programId "size"
                return $ Just res


#if defined(ghcjs_HOST_OS)
foreign import javascript safe "gl.clearColor($1,$2,$3,$4)" glClearColor :: Float -> Float -> Float -> Float -> IO ()
foreign import javascript safe "gl.blendFunc($1,$2)" glBlendFunc :: GLenum -> GLenum -> IO ()
foreign import javascript safe "gl.clear($1)" glClear :: GLuint -> IO ()
foreign import javascript safe "gl.activeTexture($1)" glActiveTexture :: GLenum -> IO ()
foreign import javascript safe "gl.disable($1)" glDisable :: GLenum -> IO ()
foreign import javascript safe "gl.enable($1)" glEnable :: GLenum -> IO ()
foreign import javascript safe "gl.bindVertexArray($1)" glBindVertexArray :: VAO -> IO ()
foreign import javascript safe "gl.bindBuffer($1,$2)" glBindBuffer :: GLenum -> VBO -> IO ()
foreign import javascript safe "gl.useProgram($1)" glUseProgram :: GLuint -> IO ()
foreign import javascript safe "$r = gl.createProgram();" glCreateProgram :: IO GLuint
foreign import javascript safe "$r = gl.getUniformLocation($1,$2);" glGetUniformLocation :: GLuint -> JSString -> IO GLint
getUniformLocation :: GLuint -> String -> IO GLint
getUniformLocation a b = glGetUniformLocation a (pack b)
foreign import javascript safe "$r = gl.getAttribLocation($1,$2);" glGetAttribLocation :: GLuint -> JSString -> IO GLuint
getAttribLocation :: GLuint -> String -> IO GLuint
getAttribLocation a b = glGetAttribLocation a (pack b)
foreign import javascript safe "gl.attachShader($1,$2)" glAttachShader :: GLuint -> GLuint -> IO ()
foreign import javascript safe "gl.bindTexture($1,$2)" glBindTexture :: GLenum -> GLint -> IO ()
foreign import javascript safe "gl.compileShader($1)" glCompileShader :: GLuint -> IO ()
foreign import javascript safe "$r = gl.createShader($1)" glCreateShader :: GLenum -> IO GLuint
foreign import javascript safe "gl.deleteShader($1)" glDeleteShader :: GLuint -> IO ()
foreign import javascript safe "gl.enableVertexAttribArray($1)" glEnableVertexAttribArray :: GLuint -> IO ()
foreign import javascript safe "gl.generateMipmap($1)" glGenerateMipmap :: GLenum -> IO ()
foreign import javascript safe "gl.unmapBuffer($1)" glUnmapBuffer :: GLenum -> IO ()
foreign import javascript safe "gl.linkProgram($1)" glLinkProgram :: GLuint -> IO ()
foreign import javascript safe "g.drawElements($1, $2, $3, 0);" glDrawElements :: GLenum -> GLuint -> GLenum -> IO ()


foreign import javascript safe "" glGetBooleanv :: IO ()
foreign import javascript safe "" glGetProgramInfoLog :: IO ()
foreign import javascript safe "" glGetProgramiv :: IO ()
foreign import javascript safe "" glGetShaderInfoLog :: IO ()
foreign import javascript safe "" glGetShaderiv :: IO ()
foreign import javascript safe "" glShaderSource :: IO ()
foreign import javascript safe "" glUniform4fv :: IO ()
foreign import javascript safe "" glUniformMatrix4fv :: IO ()
foreign import javascript safe "" glVertexAttribPointer :: IO ()

foreign import javascript safe " \
    var ext = gl.getExtension('OES_vertex_array_object'); \
    $r = ext.createVertexArrayOES(); \
    " makeVAO :: IO VAO

foreign import javascript safe " \
    var ext = gl.getExtension('OES_vertex_array_object'); \
    ext.bindVertexArrayOES($1); \
    " bindVAO :: VAO -> IO ()

foreign import javascript safe "$r = gl.createBuffer();" makeVBO :: IO VBO

foreign import javascript safe "gl.bufferData($1, new Float32Array($2), $3);" glBufferData :: GLenum -> JSVal -> GLenum -> IO ()

bufferData bufType lst usageType = do
        arr <- toJSVal lst
        glBufferData bufType arr usageType

#else

bufferData bufType lst usageType =
        withArrayLen lst $ \len ptr ->
            glBufferData bufType
                         (fromIntegral (len * sizeOf (head lst)))
                         ptr
                         usageType

getAttribLocation progId name = withCString name (glGetAttribLocation progId)
getUniformLocation progId name = withCString name (glGetUniformLocation progId)

-- VAO

makeVAO :: IO VAO
makeVAO = withNewPtr (glGenVertexArrays 1)

makeVBO :: IO VAO
makeVBO = withNewPtr (glGenBuffers 1)

bindVAO :: VAO -> IO ()
bindVAO = glBindVertexArray

-- Textures
genTexture = alloca $ \p ->
    do
        glGenTextures 1 p
        peek p

-- create linear filtered texture
loadGLTex format w h ptr = do
    tex <- genTexture
    glBindTexture GL_TEXTURE_2D tex

    glTexImage2D GL_TEXTURE_2D 0 (fromIntegral format)
        (fromIntegral w) (fromIntegral h)
        0 format GL_UNSIGNED_BYTE ptr

    glGenerateMipmap GL_TEXTURE_2D

    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_LINEAR)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_LINEAR_MIPMAP_LINEAR)
    return $ Just $ TexID (fromIntegral tex)
#endif

configGLState :: GLfloat -> GLfloat -> GLfloat -> IO ()
configGLState r g b = do
        glClearColor r g b 1
        glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        glActiveTexture GL_TEXTURE0

        glDisable GL_DITHER
        glDisable GL_STENCIL_TEST

        glEnable GL_PROGRAM_POINT_SIZE -- for OSX

        glEnable GL_BLEND

clearScreen :: IO ()
clearScreen = do
        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
