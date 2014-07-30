{-# LANGUAGE NamedFieldPuns #-}

module Graphics.Drawing where

import Types.Color
import Math.Matrix
import Foreign.Marshal.Alloc
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Graphics.GLUtils
import Foreign.Ptr
import Graphics.Rendering.OpenGL.Raw.Core31
import qualified Data.Foldable as Fold

newtype Shader = Shader CUInt deriving (Eq, Ord, Show)

nullShader :: Shader
nullShader = Shader (-1)

loadShader :: String -> String -> IO (Maybe Shader)
loadShader vertShaderName fragShaderName = 
        withCString vertShaderName $ \vname ->
            withCString fragShaderName $ \fname ->
                do
                    val <- c'loadShader vname fname
                    if val >= 0
                        then
                            return $ Just $ Shader val
                        else return Nothing

foreign import ccall "load_shader" c'loadShader
    :: CString -> CString -> IO CUInt

type VAO = CUInt
type VBO = CUInt

foreign import ccall "init_renderer" initRenderer
   :: IO ()

foreign import ccall "reset_renderer" resetRenderer
   :: IO ()

renderCommands :: Mat44 -> Label -> IO ()
renderCommands mat label = 
      withMat44 mat $ \ptr ->
         c'renderCommands ptr label

type Label = CInt

worldLabel :: Label
worldLabel = 0

boolToCInt :: Bool -> CInt
boolToCInt b = if b then 1 else 0

foreign import ccall "render_commands" c'renderCommands
   :: Mat44Raw -> CInt -> IO ()

data DrawCommandStruct = DrawCommandStruct
type DrawCommandHandle = Ptr DrawCommandStruct

addDrawCommand :: Mat44 -> Color -> Color -> TexID -> Shader -> Label -> GLfloat -> Bool -> IO (DrawCommandHandle)
addDrawCommand mat color1 color2 (TexID texid) (Shader shader) label depth blend =
      withMat44 mat $ \matptr ->
         withVec4 color1 $ \color1ptr ->
            withVec4 color2 $ \color2ptr ->
               c'addDrawCommand matptr color1ptr color2ptr texid shader label depth (boolToCInt blend)
               
foreign import ccall "add_draw_command" c'addDrawCommand
   :: Mat44Raw -> Vec4Raw -> Vec4Raw -> CInt -> CUInt -> CInt -> CFloat -> CInt -> IO (DrawCommandHandle)

setTCCommand :: DrawCommandHandle -> Vec4 -> IO ()
setTCCommand dc vec =
      withVec4 vec $ \ptr ->
         c'setTCCommand dc ptr

foreign import ccall "set_tc_command" c'setTCCommand
   :: DrawCommandHandle -> Vec4Raw -> IO ()

newtype Attribute = Attribute CInt deriving (Eq, Ord, Show)

data Attachment = Attachment Attribute CInt
data VertexGroup = VertexGroup [Attachment]

data VAOConfig = VAOConfig {
               vao :: !VAO,
               indexVBO :: Maybe VBO,
               vertices :: ![VBO],
               shader :: Shader
               } deriving (Show)

(sp_ATTR_POSITION:
 sp_ATTR_TEX_COORDS:
 sp_ATTR_COLOR:
 sp_ATTR_COLOR2:
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

        return $ VAOConfig { vao = vao', 
                           indexVBO = Nothing,
                           vertices = buffers,
                           shader = sh
                           }

halveInt :: Int -> Int
halveInt a = floor fl
    where fl = (fromIntegral a) / 2 :: Float

drawPoints :: Real a => VAOConfig -> [CFloat] -> a -> IO ()
drawPoints (VAOConfig vao Nothing [vbo] shader) points@(x:_) size = do
        glBindVertexArray vao
        bufferVertices vbo points
        unbindVAO

        dc <- addDrawCommand mat44Identity white white nullTex shader worldLabel 0.0 False
        vao_payload <- setVAOCommand dc vao (halveInt (length points)) gl_POINTS
        uniloc <- grabUniformLoc shader sp_UNIFORM_SIZE
        addFloatUniform dc uniloc [size]

drawPoints a b c = print "invalid drawpoints command" >> print a >> print b >> return ()

bufferVertices :: VBO -> [CFloat] -> IO ()
bufferVertices vbo floats = do
        let len = length floats
        ptr <- mallocArray len
        pokeArray ptr $ fmap realToFrac floats
        c'bufferVertices vbo ptr (fromIntegral len)
        free ptr

foreign import ccall "buffer_vertices_num" c'bufferVertices
    :: CUInt -> Ptr CFloat -> CInt -> IO ()

bufferSquareIndices :: VBO -> Int -> IO Int
bufferSquareIndices vbo num = do
        numindices <- c'bufferSquareIndices vbo (fromIntegral num)
        return $ fromIntegral numindices

foreign import ccall "buffer_square_indices" c'bufferSquareIndices
    :: CUInt -> CInt -> IO CUInt

data VAOPayloadStruct = VAOPayloadStruct
type VAOPayloadHandle = Ptr VAOPayloadStruct

setVAOCommand :: Integral a => DrawCommandHandle -> VAO -> a -> GLenum -> IO VAOPayloadHandle
setVAOCommand dc vao numIndices drawType =
        c'setVAOCommand dc vao (fromIntegral numIndices) drawType

foreign import ccall "set_vao_command" c'setVAOCommand
    :: DrawCommandHandle -> CUInt -> CUInt -> GLenum -> IO VAOPayloadHandle

newtype Uniform = Uniform CInt deriving (Eq, Ord, Show)
newtype UniformLoc = UniformLoc CInt deriving (Eq, Ord, Show)

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
addFloatUniform dc (UniformLoc u) floats = do
        let len = length floats
        ptr <- mallocArray len
        pokeArray ptr $ fmap realToFrac floats
        c'addFloatUniform dc u (fromIntegral len) ptr
        free ptr

foreign import ccall "add_float_uniform" c'addFloatUniform
    :: DrawCommandHandle -> GLint -> CUInt -> (Ptr CFloat) -> IO ()

foreign import ccall "make_vao" makeVAO
    :: IO VAO

foreign import ccall "make_vbo" makeVBO
    :: IO VBO

foreign import ccall "attach_vertex_array" attachVertexArray
    :: CUInt -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall "getMainVAO" getMainVAO
    :: IO CUInt

