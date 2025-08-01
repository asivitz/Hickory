{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Hickory.Vulkan.Monad where

import GHC.Generics (Generic)
import Control.Lens (view)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT (..), runReaderT, ask, MonadReader, local, mapReaderT)
import Control.Monad.State.Class (modify)
import Control.Monad.State.Strict (StateT (..), evalStateT, get)
import Control.Monad.Writer.Strict (WriterT (..), runWriterT, tell, mapWriterT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import Data.Maybe (fromMaybe)
import Data.Word (Word32, Word64)
import Foreign (Storable, sizeOf)
import Hickory.Graphics.DrawText (squareIndices)
import Hickory.Text.Text (transformTextCommandToVerts)
import Hickory.Vulkan.DescriptorSet (BufferDescriptorSet(..), withBufferDescriptorSet)
import Hickory.Vulkan.Framing (FramedResource, frameResource)
import Hickory.Vulkan.Material (withMaterial, PipelineOptions(..))
import Hickory.Vulkan.DynamicMesh (DynamicBufferedMesh(..), uploadDynamicMesh)
import Vulkan
  ( CommandBuffer, cmdBindVertexBuffers, cmdDraw, cmdBindIndexBuffer, cmdDrawIndexed, IndexType(..), Buffer
  , DescriptorSetLayout
  )
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.ByteString as B
import Acquire (Acquire)
import Hickory.Text.ParseJson (Font)
import Hickory.Text.Types (TextCommand)
import Hickory.Vulkan.Types (Material (..), PointedDescriptorSet, RenderConfig (..), VulkanResources, Attribute (..), FrameContext, Mesh (..))
import Linear (liftI2, zero)
import GHC.List (foldl1')


data BufferedUniformMaterial pushConst uniform = BufferedUniformMaterial
  { material    :: Material pushConst
  , descriptor  :: FramedResource (BufferDescriptorSet uniform)
  , uniformSize :: Int -- Bytes
  } deriving Generic

withBufferedUniformMaterial
  :: forall pushConst uniform
  .  (Storable uniform, Storable pushConst)
  => VulkanResources
  -> RenderConfig
  -> [Attribute]
  -> PipelineOptions
  -> B.ByteString
  -> B.ByteString
  -> FramedResource PointedDescriptorSet -- Global descriptor set
  -> Maybe DescriptorSetLayout -- Per draw descriptor set
  -> Acquire (BufferedUniformMaterial pushConst uniform)
withBufferedUniformMaterial vulkanResources renderConfig attributes pipelineOptions vert frag globalDescriptorSet perDrawDescriptorSetLayout = do
  descriptor <- frameResource $ withBufferDescriptorSet vulkanResources 2048
  let
    materialSet = view #descriptorSet <$> descriptor
    uniformSize = sizeOf (undefined :: uniform)
  material <- withMaterial vulkanResources renderConfig attributes pipelineOptions pipelineOptions.cullMode vert frag
    [ globalDescriptorSet
    , materialSet
    ]
    perDrawDescriptorSetLayout
  pure BufferedUniformMaterial {..}

{- Batch IO Monad -}
class Monad m => BatchIOMonad m where
  recordIO :: IO () -> m () -- record IO action to be later executed in a batch

newtype BatchIOT m a = BatchIOT { unBatchIOT :: WriterT (IO ()) m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance Monad m => BatchIOMonad (BatchIOT m) where
  recordIO = BatchIOT . tell

runBatchIO :: MonadIO m => BatchIOT m a -> m a
runBatchIO f = do
  (a, io :: IO ()) <- runWriterT . unBatchIOT $ f
  liftIO io
  pure a

{- Frame Monad -}

class Monad m => FrameMonad m where
  askFrameContext  :: m FrameContext

newtype FrameT m a = FrameT { unFrameT :: ReaderT FrameContext m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance Monad m => FrameMonad (FrameT m) where
  askFrameContext   = FrameT ask

runFrame :: FrameContext -> FrameT m a -> m a
runFrame fc = flip runReaderT fc . unFrameT

mapFrameT :: (m a -> n b) -> FrameT m a -> FrameT n b
mapFrameT f = FrameT . mapReaderT f . unFrameT

{- Dynamic Mesh Monad -}

class Monad m => DynamicMeshMonad m where
  askDynamicMesh :: m DynamicBufferedMesh
  getMeshes      :: m [Mesh]
  addMesh        :: Mesh -> m ()

useDynamicMesh :: MonadIO m => DynamicBufferedMesh -> DynamicMeshT m a -> m a
useDynamicMesh dynamicMesh f = flip runReaderT dynamicMesh . flip evalStateT mempty . unDynamicMeshT $ do
  a <- f
  meshes <- getMeshes
  uploadDynamicMesh dynamicMesh (reverse meshes)
  pure a

newtype DynamicMeshT m a = DynamicMeshT { unDynamicMeshT :: StateT [Mesh] (ReaderT DynamicBufferedMesh m) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance MonadTrans DynamicMeshT where
  lift = DynamicMeshT . lift . lift

instance Monad m => DynamicMeshMonad (DynamicMeshT m) where
  askDynamicMesh = DynamicMeshT ask
  getMeshes      = DynamicMeshT get
  addMesh mesh   = DynamicMeshT $ modify (mesh:)

mapStateT' :: (Functor m, Functor n) => (m a -> n b) -> StateT s m a -> StateT s n b
mapStateT' f m = StateT $ \s -> (,s) <$> f (fst <$> runStateT m s)

mapDynamicMeshT :: forall m n a b. (Monad m, Monad n) => (m a -> n b) -> DynamicMeshT m a -> DynamicMeshT n b
mapDynamicMeshT f = DynamicMeshT . mapStateT' (mapReaderT f) . unDynamicMeshT

{- Transitive Instances -}

instance {-# OVERLAPPABLE #-} (MonadTrans t, FrameMonad m, Monad (t m))            => FrameMonad (t m)   where askFrameContext = lift askFrameContext
instance {-# OVERLAPPABLE #-} (MonadTrans t, BatchIOMonad m, Monad (t m))          => BatchIOMonad (t m) where recordIO = lift . recordIO
instance {-# OVERLAPPABLE #-} (MonadTrans t, DynamicMeshMonad m, Monad (t m))      => DynamicMeshMonad (t m) where
  askDynamicMesh = lift askDynamicMesh
  getMeshes      = lift getMeshes
  addMesh m      = lift $ addMesh m

instance MonadReader r m => MonadReader r (DynamicMeshT m) where
  ask = lift ask
  local f = mapDynamicMeshT id . local f

instance MonadReader r m => MonadReader r (FrameT m) where
  ask = lift ask
  local f = mapFrameT id . local f

instance MonadReader r m => MonadReader r (BatchIOT m) where
  ask = lift ask
  local f = BatchIOT . mapWriterT id . unBatchIOT . local f

{- Utilities -}

packVecs :: (Storable a, Foldable f) => [f a] -> SV.Vector a
packVecs = SV.fromList . concatMap toList

textMesh :: Font -> TextCommand -> Mesh
textMesh font tc = case numSquares of
  0 -> Mesh { indices = Nothing, vertices = [], minPosition = zero, maxPosition = zero, morphTargets = [] }
  _ -> Mesh { indices = Just (SV.fromList indices)
            , vertices = [(Position, packVecs posVecs), (TextureCoord, packVecs tcVecs)]
            , minPosition, maxPosition, morphTargets = []
            }
  where
  (numSquares, posVecs, tcVecs) = transformTextCommandToVerts tc font
  (indices, _numBlockIndices)   = squareIndices (fromIntegral numSquares)
  minPosition = foldl1' (liftI2 min) posVecs
  maxPosition = foldl1' (liftI2 max) posVecs

cmdDrawBufferedMesh :: MonadIO m => CommandBuffer -> Material a -> Word32 -> [(Attribute, Word32)] -> Buffer -> Word32 -> Maybe Word32 -> Word32 -> Word64 -> Maybe Buffer -> Maybe String -> m ()
cmdDrawBufferedMesh commandBuffer Material {..} (fromIntegral -> vertexOffset) meshOffsets vertexBuffer instanceCount mNumIndices numVertices indexOffset mIndexBuffer mName = do
  let
      bindOffsets = V.fromList $ attributes <&> \a -> (+vertexOffset) . fromIntegral . fromMaybe (error $ "Can't find attribute '" ++ show a ++ "' in mesh " ++ fromMaybe "" mName)
                                                    $ Prelude.lookup a meshOffsets
      vertexBuffers = V.fromList $ vertexBuffer <$ attributes

  cmdBindVertexBuffers commandBuffer 0 vertexBuffers bindOffsets

  case (mNumIndices, mIndexBuffer) of
    (Just n, Just ibuf) -> do
      cmdBindIndexBuffer commandBuffer ibuf indexOffset INDEX_TYPE_UINT32
      cmdDrawIndexed commandBuffer n instanceCount 0 0 0
    (Nothing, Nothing) -> do
      cmdDraw commandBuffer numVertices instanceCount 0 0
    _ -> error "Mesh has indices but they aren't buffered."
