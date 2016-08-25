{-# LANGUAGE NamedFieldPuns #-}

module Graphics.Drawing (
                        DrawType(..),
                        Shader(..),
                        Attribute,
                        Attachment(..),
                        VertexGroup(..),
                        squareIndices,
                        VAO,
                        VBO,
                        VAOConfig(..),
                        getShader,
                        TexID(..),
                        getTexID,
                        ShaderID,
                        ProgramID
                        )
                        where

import Data.Word
import Data.Int

#if defined(ghcjs_HOST_OS)
import GHCJS.Types
#endif

#if defined(ghcjs_HOST_OS)
instance Show JSVal
        where show x = "Some JSVal"

type VAO = JSVal
type VBO = JSVal
type ShaderID = JSVal
type ProgramID = JSVal

newtype TexID = TexID JSVal deriving (Show)
#else
type VAO = Word32
type VBO = Word32
type ShaderID = Word32
type ProgramID = Word32

newtype TexID = TexID Word32 deriving (Show)
#endif

getTexID (TexID num) = num


data DrawType = TriangleFan | TriangleStrip | Triangles
              deriving (Show, Eq)


data Shader = Shader {
            program :: ProgramID,
            vertShader :: ShaderID,
            fragShader :: ShaderID,

            sp_ATTR_POSITION :: Word32,
            sp_ATTR_TEX_COORDS :: Word32,
            sp_ATTR_COLOR :: Word32,
            sp_ATTR_COLOR2 :: Word32,
            sp_ATTR_NORMALS :: Word32,

            sp_UNIFORM_TEXID :: Int32,
            sp_UNIFORM_COLOR :: Int32,
            sp_UNIFORM_COLOR2 :: Int32,
            sp_UNIFORM_MODEL_MAT :: Int32,
            sp_UNIFORM_VIEW_MAT :: Int32,
            sp_UNIFORM_SIZE :: Int32
            } deriving (Show)

getShader Shader { program } = program

type Attribute = Shader -> Word32

data Attachment = Attachment Attribute Int32
data VertexGroup = VertexGroup [Attachment]

data VAOConfig = VAOConfig {
               vao :: !VAO,
               indexVBO :: Maybe VBO,
               vertices :: ![VBO]
               } deriving (Show)

{-
halveInt :: Int -> Int
halveInt a = floor fl
    where fl = (fromIntegral a) / 2 :: Float

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
