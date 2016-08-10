{-# LANGUAGE NamedFieldPuns #-}

module Graphics.Drawing ( module Graphics.GLUtils,
                          RenderLayer,
                          worldLayer,
                          uiRenderLayer,
                          backgroundLayer,
                          drawCommand,
                          Attachment(..),
                          VertexGroup(..),
                          VAOConfig(..),
                          createVAOConfig,
                          indexVAOConfig,
                          squareIndices,
                          bufferVertices,
                          bufferIndices,
                          bindVAO
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
import Graphics.GLSupport
import qualified Data.Foldable as Fold

type VAO = GLuint
type VBO = GLuint

type RenderLayer = CInt

worldLayer :: RenderLayer
worldLayer = 0

uiRenderLayer :: RenderLayer
uiRenderLayer = 1

backgroundLayer :: RenderLayer
backgroundLayer = 3

boolToCInt :: Bool -> CInt
boolToCInt b = if b then 1 else 0

drawCommand :: Shader -> Mat44 -> Color -> Color -> Maybe TexID -> VAO -> GLint -> GLenum -> IO ()
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

        glDrawElements drawType numitems GL_UNSIGNED_SHORT nullPtr

type Attribute = Shader -> GLuint

data Attachment = Attachment Attribute GLint
data VertexGroup = VertexGroup [Attachment]

data VAOConfig = VAOConfig {
               vao :: !VAO,
               indexVBO :: Maybe VBO,
               vertices :: ![VBO]
               } deriving (Show)

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

bindVAO :: GLuint -> IO ()
bindVAO = glBindVertexArray

createVAOConfig :: Shader -> [VertexGroup] -> IO VAOConfig
createVAOConfig sh vertexgroups = do
        vao' <- makeVAO
        glBindVertexArray vao'

        buffers <- mapM (buildVertexGroup sh) vertexgroups

        return VAOConfig { vao = vao',
                         indexVBO = Nothing,
                         vertices = buffers
                         }

halveInt :: Int -> Int
halveInt a = floor fl
    where fl = (fromIntegral a) / 2 :: Float

{-
drawPoints :: Real a => Shader -> VAOConfig -> [CFloat] -> a -> IO ()
drawPoints shader (VAOConfig vao Nothing [vbo]) points@(x:_) size = do
        glBindVertexArray vao
        bufferVertices vbo points
        unbindVAO

        dc <- addDrawCommand identity white white nullTex shader worldLayer 0.0 False
        vao_payload <- setVAOCommand dc vao (halveInt (length points)) GL_POINTS
        uniloc <- grabUniformLoc shader sp_UNIFORM_SIZE
        addFloatUniform dc uniloc [size]

drawPoints _ a b c = print "invalid drawpoints command" >> print a >> print b >> return ()
        -}

bufferVertices :: VBO -> [CFloat] -> IO ()
bufferVertices vbo floats = do
        glBindBuffer GL_ARRAY_BUFFER vbo
        withArrayLen floats $ \len ptr ->
            glBufferData GL_ARRAY_BUFFER
                         (fromIntegral (len * sizeOf (0::GLfloat)))
                         ptr
                         GL_STREAM_DRAW
        _ <- glUnmapBuffer GL_ARRAY_BUFFER
        return ()

bufferIndices :: VBO -> [CUShort] -> IO ()
bufferIndices vbo ints = do
        glBindBuffer GL_ELEMENT_ARRAY_BUFFER vbo

        withArrayLen ints $ \len ptr ->
            glBufferData GL_ELEMENT_ARRAY_BUFFER
                        (fromIntegral (len * sizeOf (0::GLushort)))
                        ptr
                        GL_STREAM_DRAW

        _ <- glUnmapBuffer GL_ELEMENT_ARRAY_BUFFER
        return ()

data VAOPayloadStruct = VAOPayloadStruct
type VAOPayloadHandle = Ptr VAOPayloadStruct

withNewPtr :: Storable b => (Ptr b -> IO a) -> IO b
withNewPtr f = alloca (\p -> f p >> peek p)

makeVAO :: IO VAO
makeVAO = withNewPtr (glGenVertexArrays 1)

makeVBO :: IO VAO
makeVBO = withNewPtr (glGenBuffers 1)


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
