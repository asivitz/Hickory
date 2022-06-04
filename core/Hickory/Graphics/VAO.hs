{-# LANGUAGE FlexibleContexts, RecordWildCards #-}

module Hickory.Graphics.VAO
  ( VAOCache
  , deleteVAOConfigs
  , createIndexedVAO
  , createDirectVAO
  , VAO(..)
  , VAOConfig(..)
  , glenumForDrawType
  , VertexGroup(..)
  , Attachment(..)
  , drawElements
  ) where

import qualified Data.HashMap.Strict as Map
import Data.Text (Text)
import qualified Data.Vector.Storable as V
import Graphics.GL.Compatibility41
import Hickory.Graphics.GLSupport (VAOId, VBOId, alloc1, bufferIndices, bufferVertices, DrawType (..))
import Hickory.Graphics.Shader (Shader (program), getAttribLocation)
import GHC.Int (Int32)
import qualified Data.Foldable as Fold
import Foreign.Storable (sizeOf)
import Control.Monad (when)
import GHC.Ptr (nullPtr)
import Foreign (plusPtr)

data VAOConfig = VAOConfig
  { vaoId :: !VAOId
  , indexVBO :: Maybe VBOId
  , vertices :: ![VBOId]
  , shader :: Shader
  }
  deriving (Show)

data VAO = VAO
  { vaoConfig :: VAOConfig
  , count :: Int
  , drawType :: DrawType
  } deriving (Show)

type VAOCache = Map.HashMap Text VAO

data Attachment = Attachment String Int32

newtype VertexGroup = VertexGroup [Attachment]

instance Show VertexGroup where show _ = "Vertex Group"

createIndexedVAOConfig :: Shader -> [VertexGroup] -> IO VAOConfig
createIndexedVAOConfig sh vertexgroups = do
  vao' <- alloc1 glGenVertexArrays
  glBindVertexArray vao'

  index_vbo <- alloc1 glGenBuffers
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER index_vbo

  buffers <- mapM (buildVertexGroup sh) vertexgroups

  pure VAOConfig
    { vaoId = vao'
    , indexVBO = Just index_vbo
    , vertices = buffers
    , shader = sh
    }

createDirectVAOConfig :: Shader -> [VertexGroup] -> IO VAOConfig
createDirectVAOConfig sh vertexgroups = do
  vao' <- alloc1 glGenVertexArrays
  glBindVertexArray vao'

  buffers <- mapM (buildVertexGroup sh) vertexgroups

  pure VAOConfig
    { vaoId = vao'
    , indexVBO = Nothing
    , vertices = buffers
    , shader = sh
    }

deleteVAOConfigs :: [VAOConfig] -> IO ()
deleteVAOConfigs vcs = do
  let vaos = map (\VAOConfig {..} -> vaoId) vcs
      vaosv = V.fromList vaos
      vbos = concatMap (\VAOConfig {..} -> maybe id (:) indexVBO vertices) vcs
      vbosv = V.fromList vbos
  V.unsafeWith vaosv $ glDeleteVertexArrays (fromIntegral $ V.length vaosv)
  V.unsafeWith vbosv $ glDeleteBuffers (fromIntegral $ V.length vbosv)

loadVerticesIntoIndexedVAOConfig :: VAOConfig -> V.Vector GLfloat -> V.Vector GLuint -> IO ()
loadVerticesIntoIndexedVAOConfig VAOConfig { vaoId, indexVBO = Just ivbo, vertices = (vbo:_) } vs indices = do
  glBindVertexArray vaoId
  bufferVertices vbo vs
  bufferIndices ivbo indices
loadVerticesIntoIndexedVAOConfig _ _ _ = error "VAOConfig missing buffers"

loadVerticesIntoDirectVAOConfig :: VAOConfig -> V.Vector GLfloat -> IO ()
loadVerticesIntoDirectVAOConfig VAOConfig { vaoId, indexVBO = Nothing, vertices = (vbo:_) } vs = do
  glBindVertexArray vaoId
  bufferVertices vbo vs
loadVerticesIntoDirectVAOConfig _ _ = error "Can't load into direct vao config"

createIndexedVAO :: Shader -> [VertexGroup] -> (V.Vector GLfloat, V.Vector GLuint) -> DrawType -> IO VAO
createIndexedVAO shader vgroups (verts,indices) drawType = do
  vc <- createIndexedVAOConfig shader vgroups
  loadVerticesIntoIndexedVAOConfig vc verts indices
  pure $ VAO vc (V.length indices) drawType

createDirectVAO :: Shader -> [VertexGroup] -> (V.Vector GLfloat, Int) -> DrawType -> IO VAO
createDirectVAO shader vgroups (verts, numVerts) drawType = do
  vc <- createDirectVAOConfig shader vgroups
  loadVerticesIntoDirectVAOConfig vc verts
  pure $ VAO vc numVerts drawType

-- VAO / VBO

glenumForDrawType :: DrawType -> GLenum
glenumForDrawType dt = case dt of
  TriangleStrip -> GL_TRIANGLE_STRIP
  TriangleFan -> GL_TRIANGLE_FAN
  Triangles -> GL_TRIANGLES

attachVertexArray :: GLint -> GLint -> GLint -> GLint -> IO ()
attachVertexArray attrLoc len stride offset =
  if attrLoc < 0
    then print ("Can't attach vertex array: Bad attribute location" :: String)
    else do
      enableVertexAttribArray attrLoc
      vertexAttribPointer attrLoc len GL_FLOAT GL_FALSE (stride * fsize) (fromIntegral $ offset * fsize)
  where
    fsize :: GLint
    fsize = fromIntegral $ sizeOf (0 :: GLfloat)

buildVertexGroup :: Shader -> VertexGroup -> IO VBOId
buildVertexGroup shader group = do
  vbo <- alloc1 glGenBuffers
  attachVertexGroup shader vbo group
  pure vbo

attachVertexGroup :: Shader -> VBOId -> VertexGroup -> IO ()
attachVertexGroup shader vbo (VertexGroup attachments) = do
  glBindBuffer GL_ARRAY_BUFFER vbo

  let stride = sum $ map (\(Attachment _a l) -> l) attachments

  _ <-
    Fold.foldlM
      ( \offset (Attachment a l) -> do
          loc <- getAttribLocation (program shader) a
          when (loc >= 0) do
            attachVertexArray loc l stride offset
          return (offset + l)
      )
      0
      attachments
  pure ()

enableVertexAttribArray :: GLint -> IO ()
enableVertexAttribArray = glEnableVertexAttribArray . fromIntegral

disableVertexAttribArray :: GLint -> IO ()
disableVertexAttribArray = glDisableVertexAttribArray . fromIntegral

drawElements :: GLenum -> GLsizei -> GLenum -> IO ()
drawElements a b c = glDrawElements a b c nullPtr

vertexAttribPointer :: GLint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLint -> IO ()
vertexAttribPointer a b c d e f = glVertexAttribPointer (fromIntegral a) b c d e (plusPtr nullPtr (fromIntegral f))
