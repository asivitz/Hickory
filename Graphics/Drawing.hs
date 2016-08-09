{-# LANGUAGE NamedFieldPuns #-}

module Graphics.Drawing ( module Graphics.GLUtils,
                          renderCommands,
                          RenderLayer,
                          worldLayer,
                          uiRenderLayer,
                          backgroundLayer,
                          addDrawCommand,
                          drawCommand,
                          setTCCommand,
                          setVAOCommand,
                          getUniformLoc,
                          grabUniformLoc,
                          addFloatUniform,
                          Uniform,
                          UniformLoc,
                          Attachment(..),
                          VertexGroup(..),
                          VAOConfig(..),
                          drawPoints,
                          initRenderer,
                          resetRenderer,
                          createVAOConfig,
                          indexVAOConfig,
                          bindVAO,
                          squareIndices,
                          bufferVertices,
                          bufferIndices,
                          unbindVAO,
                          sp_ATTR_POSITION,
                          sp_ATTR_TEX_COORDS,
                          sp_ATTR_COLOR,
                          sp_ATTR_COLOR2,
                          sp_ATTR_NORMALS,
                          sp_UNIFORM_TEXID,
                          sp_UNIFORM_COLOR,
                          sp_UNIFORM_COLOR2,
                          sp_UNIFORM_MODEL_MAT,
                          sp_UNIFORM_VIEW_MAT,
                          sp_UNIFORM_SIZE
                        )
                        where

import Types.Color
import Math.Matrix
import Math.Vector
import Math.VectorMatrix
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Graphics.GLUtils
import Graphics.Shader
import Foreign.Ptr
import Graphics.Rendering.OpenGL.Raw.Core31
import qualified Data.Foldable as Fold

type VAO = CUInt
type VBO = CUInt

foreign import ccall "init_renderer" initRenderer
   :: IO ()

foreign import ccall "reset_renderer" resetRenderer
   :: IO ()

renderCommands :: Mat44 -> RenderLayer -> IO ()
renderCommands mat layer =
      withMat44 mat $ \ptr ->
         c'renderCommands ptr layer

getUniformLoc (Shader s) name =
        withCString name $ \ptrname ->
            glGetUniformLocation s ptrname

type RenderLayer = CInt

worldLayer :: RenderLayer
worldLayer = 0

uiRenderLayer :: RenderLayer
uiRenderLayer = 1

backgroundLayer :: RenderLayer
backgroundLayer = 3

boolToCInt :: Bool -> CInt
boolToCInt b = if b then 1 else 0

foreign import ccall "render_commands" c'renderCommands
   :: Mat44Raw -> CInt -> IO ()

data DrawCommandStruct = DrawCommandStruct
type DrawCommandHandle = Ptr DrawCommandStruct

foreign import ccall "use_shader" useShader :: CInt -> IO ()

drawCommand :: Shader -> Mat44 -> Color -> Color -> Maybe TexID -> VAO -> CInt -> GLenum -> IO ()
drawCommand shader mat color color2 texid vao numitems drawType = do
        useShader (fromIntegral $ getShader shader)
        case texid of
            Just t -> glBindTexture gl_TEXTURE_2D (fromIntegral $ getTexID t)
            _ -> return ()

        colLoc <- getUniform <$> grabUniformLoc shader sp_UNIFORM_COLOR
        col2Loc <- getUniform <$> grabUniformLoc shader sp_UNIFORM_COLOR2

        withVec4 color $ \ptr ->
            glUniform4fv colLoc 1 ptr
        withVec4 color2 $ \ptr ->
            glUniform4fv col2Loc 1 ptr

        modelMatLoc <- getUniform <$> grabUniformLoc shader sp_UNIFORM_MODEL_MAT
        viewMatLoc <- getUniform <$> grabUniformLoc shader sp_UNIFORM_VIEW_MAT
        withMat44 mat $ \ptr ->
            glUniformMatrix4fv modelMatLoc 1 (fromIntegral gl_FALSE) ptr
        withMat44 identity $ \ptr ->
            glUniformMatrix4fv viewMatLoc 1 (fromIntegral gl_FALSE) ptr
        glBindVertexArray vao

        glDrawElements drawType numitems gl_UNSIGNED_SHORT nullPtr

addDrawCommand :: Mat44 -> Color -> Color -> TexID -> Shader -> RenderLayer -> GLfloat -> Bool -> IO DrawCommandHandle
addDrawCommand mat color1 color2 (TexID texid) (Shader shader) layer depth blend =
      withMat44 mat $ \matptr ->
         withVec4 color1 $ \color1ptr ->
            withVec4 color2 $ \color2ptr ->
               c'addDrawCommand matptr color1ptr color2ptr texid shader layer depth (boolToCInt blend)

foreign import ccall "add_draw_command" c'addDrawCommand
   :: Mat44Raw -> Ptr CFloat -> Ptr CFloat -> CInt -> CUInt -> CInt -> CFloat -> CInt -> IO DrawCommandHandle

setTCCommand :: DrawCommandHandle -> V4 Scalar -> IO ()
setTCCommand dc vec =
      withVec4 vec $ \ptr ->
         c'setTCCommand dc ptr

foreign import ccall "set_tc_command" c'setTCCommand
   :: DrawCommandHandle -> Ptr CFloat -> IO ()

newtype Attribute = Attribute CInt deriving (Eq, Ord, Show)

data Attachment = Attachment Attribute CInt
data VertexGroup = VertexGroup [Attachment]

data VAOConfig = VAOConfig {
               vao :: !VAO,
               indexVBO :: Maybe VBO,
               vertices :: ![VBO]
               } deriving (Show)

(sp_ATTR_POSITION:
 sp_ATTR_TEX_COORDS:
 sp_ATTR_COLOR:
 sp_ATTR_COLOR2:
 sp_ATTR_NORMALS:
 sp_ATTR_MAXNUM) = map Attribute [0..]

buildVertexGroup :: Shader -> VertexGroup -> IO VBO
buildVertexGroup (Shader shader) (VertexGroup attachments) = do
        vbo <- makeVBO
        glBindBuffer gl_ARRAY_BUFFER vbo

        let stride = foldl (+) 0 $ map (\(Attachment a l) -> l) attachments

        Fold.foldlM (\offset (Attachment (Attribute a) l) -> do
                attachVertexArray shader a l stride offset
                return (offset + l))
            0
            attachments

        return vbo

indexVAOConfig :: VAOConfig -> IO VAOConfig
indexVAOConfig config@VAOConfig { vao } = do
        glBindVertexArray vao
        index_vbo <- makeVBO
        glBindBuffer gl_ELEMENT_ARRAY_BUFFER index_vbo

        unbindVAO
        return $ config { indexVBO = Just index_vbo }

bindVAO = glBindVertexArray

unbindVAO = getMainVAO >>= glBindVertexArray

createVAOConfig :: Shader -> [VertexGroup] -> IO VAOConfig
createVAOConfig sh vertexgroups = do
        vao' <- makeVAO
        glBindVertexArray vao'

        buffers <- mapM (buildVertexGroup sh) vertexgroups

        unbindVAO

        return VAOConfig { vao = vao',
                         indexVBO = Nothing,
                         vertices = buffers
                         }

halveInt :: Int -> Int
halveInt a = floor fl
    where fl = (fromIntegral a) / 2 :: Float

drawPoints :: Real a => Shader -> VAOConfig -> [CFloat] -> a -> IO ()
drawPoints shader (VAOConfig vao Nothing [vbo]) points@(x:_) size = do
        glBindVertexArray vao
        bufferVertices vbo points
        unbindVAO

        dc <- addDrawCommand identity white white nullTex shader worldLayer 0.0 False
        vao_payload <- setVAOCommand dc vao (halveInt (length points)) gl_POINTS
        uniloc <- grabUniformLoc shader sp_UNIFORM_SIZE
        addFloatUniform dc uniloc [size]

drawPoints _ a b c = print "invalid drawpoints command" >> print a >> print b >> return ()

bufferVertices :: VBO -> [CFloat] -> IO ()
bufferVertices vbo floats = do
        glBindBuffer gl_ARRAY_BUFFER vbo
        withArrayLen floats $ \len ptr ->
            glBufferData gl_ARRAY_BUFFER
                         (fromIntegral (len * sizeOf (0::GLfloat)))
                         ptr
                         gl_STREAM_DRAW
        _ <- glUnmapBuffer gl_ARRAY_BUFFER
        return ()

bufferIndices :: VBO -> [CUShort] -> IO ()
bufferIndices vbo ints = do
        glBindBuffer gl_ELEMENT_ARRAY_BUFFER vbo

        withArrayLen ints $ \len ptr ->
            glBufferData gl_ELEMENT_ARRAY_BUFFER
                        (fromIntegral (len * sizeOf (0::GLushort)))
                        ptr
                        gl_STREAM_DRAW

        _ <- glUnmapBuffer gl_ELEMENT_ARRAY_BUFFER
        return ()

data VAOPayloadStruct = VAOPayloadStruct
type VAOPayloadHandle = Ptr VAOPayloadStruct

setVAOCommand :: Integral a => DrawCommandHandle -> VAO -> a -> GLenum -> IO VAOPayloadHandle
setVAOCommand dc vao numIndices drawType = c'setVAOCommand dc vao (fromIntegral numIndices) drawType

foreign import ccall "set_vao_command" c'setVAOCommand
    :: DrawCommandHandle -> CUInt -> CUInt -> GLenum -> IO VAOPayloadHandle

newtype Uniform = Uniform CInt deriving (Eq, Ord, Show)
newtype UniformLoc = UniformLoc CInt deriving (Eq, Ord, Show)

getUniform (UniformLoc loc) = loc

(sp_UNIFORM_TEXID:
 sp_UNIFORM_COLOR:
 sp_UNIFORM_COLOR2:
 sp_UNIFORM_MODEL_MAT:
 sp_UNIFORM_VIEW_MAT:
 sp_UNIFORM_SIZE:
 sp_UNIFORM_MAXNUM) = map Uniform [0..]

foreign import ccall "grab_uniform_loc" c'grabUniformLoc
    :: CUInt -> CInt -> IO CInt

grabUniformLoc :: Shader -> Uniform -> IO UniformLoc
grabUniformLoc (Shader shader) (Uniform uniform) = do
        loc <- c'grabUniformLoc shader uniform
        return $ UniformLoc loc

addFloatUniform :: Real a => DrawCommandHandle -> UniformLoc -> [a] -> IO ()
addFloatUniform dc (UniformLoc u) floats =
        withArrayLen (map realToFrac floats) $ \len ptr ->
            c'addFloatUniform dc u (fromIntegral len) ptr

foreign import ccall "add_float_uniform" c'addFloatUniform
    :: DrawCommandHandle -> GLint -> CUInt -> (Ptr CFloat) -> IO ()

withNewPtr :: Storable b => (Ptr b -> IO a) -> IO b
withNewPtr f = alloca (\p -> f p >> peek p)

makeVAO :: IO VAO
makeVAO = withNewPtr (glGenVertexArrays 1)

makeVBO :: IO VAO
makeVBO = withNewPtr (glGenBuffers 1)


foreign import ccall "attach_vertex_array" attachVertexArray
    :: CUInt -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall "getMainVAO" getMainVAO
    :: IO CUInt

squareIndices :: (Num a, Enum a, Ord a) => a -> ([a], a)
squareIndices numSquares = (indices, 4 * numSquares + 2 * (numSquares - 1))
        where indices = concat $ (flip map) [0..(numSquares - 1)]
                                 (\i -> let items = [i * 4,
                                                     i * 4 + 1,
                                                     i * 4 + 2,
                                                     i * 4 + 3]
                                            -- We need to start and end degenerate squares if
                                            -- we're not at the beginning/end
                                            withStartOfDegenerateSquare = if i < numSquares - 1 then items ++ [i * 4 + 3] else items
                                            in  if i > 0 then (i * 4) : withStartOfDegenerateSquare else withStartOfDegenerateSquare)
