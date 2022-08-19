{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Hickory.Vulkan.Monad where

import GHC.Generics (Generic)
import Control.Lens (view)
import Control.Monad (when, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT (..), runReaderT, ask, MonadReader, local, mapReaderT)
import Control.Monad.State.Class (modify, MonadState)
import Control.Monad.State.Strict (StateT (..), evalStateT, get, put, mapStateT)
import Control.Monad.Writer.Strict (WriterT (..), runWriterT, tell, mapWriterT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Foldable (foldrM, toList, for_)
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import Data.IORef (modifyIORef', readIORef)
import Data.List (sortOn, mapAccumL)
import Data.Map (Map, lookup)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import Data.UUID (UUID)
import Data.Word (Word32, Word64)
import Foreign (Storable)
import Hickory.Graphics.DrawText (squareIndices)
import Hickory.Graphics.MatrixMonad (MatrixT)
import Hickory.Text.Text (TextCommand, Font, transformTextCommandToVerts)
import Hickory.Vulkan.DescriptorSet (PointedDescriptorSet (..), TextureDescriptorSet (..), BufferDescriptorSet(..), uploadBufferDescriptor, withBufferDescriptorSet, PointedDescriptorSet)
import Hickory.Vulkan.Frame (FrameContext (..))
import Hickory.Vulkan.Framing (resourceForFrame, FramedResource, frameResource, doubleResource)
import Hickory.Vulkan.Material (Material (..), cmdPushMaterialConstants, cmdBindMaterial, withMaterial, cmdBindDrawDescriptorSet)
import Hickory.Vulkan.Mesh (BufferedMesh (..), vsizeOf, attrLocation, Mesh(..), numVerts, Attribute(..))
import Hickory.Vulkan.Text (DynamicBufferedMesh(..), uploadDynamicMesh)
import Vulkan (CommandBuffer, cmdBindVertexBuffers, cmdDraw, cmdBindIndexBuffer, cmdDrawIndexed, IndexType(..), Buffer, PrimitiveTopology(..))
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Hickory.Vulkan.Vulkan (VulkanResources, Swapchain)
import qualified Data.ByteString as B
import Acquire.Acquire (Acquire)


data BufferedUniformMaterial uniform = BufferedUniformMaterial
  { material   :: Material
  , descriptor :: FramedResource (BufferDescriptorSet uniform)
  } deriving Generic

withBufferedUniformMaterial
  :: Storable uniform
  => VulkanResources
  -> Swapchain
  -> [Attribute]
  -> B.ByteString
  -> B.ByteString
  -> Maybe PointedDescriptorSet -- Global descriptor set
  -> Maybe PointedDescriptorSet -- Per draw descriptor set
  -> Acquire (BufferedUniformMaterial uniform)
withBufferedUniformMaterial vulkanResources swapchain attributes vert frag globalDescriptorSet perDrawDescriptorSet = do
  descriptor <- frameResource $ withBufferDescriptorSet vulkanResources
  let
    materialSets = maybe id (:) (doubleResource <$> globalDescriptorSet) [view #descriptorSet <$> descriptor]
  material <- withMaterial vulkanResources swapchain attributes PRIMITIVE_TOPOLOGY_TRIANGLE_LIST vert frag materialSets (doubleResource <$> perDrawDescriptorSet)
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

{- Command Monad -}

class (FrameMonad m, BatchIOMonad m) => CommandMonad m where
  recordDrawCommand
    :: Bool -- True if blended
    -> Material
    -> Word32
    -> (CommandBuffer -> IO ())
    -> m ()

recordCommandBuffer :: MonadIO m => CommandT m a -> m a
recordCommandBuffer f = flip evalStateT (DrawCommands mempty mempty mempty) $ do
  a <- unCommandT f
  DrawCommands {..} <- get

  let allCommands = blendedCommands ++ sortOn fst opaqueCommands
      bindMat :: UUID -> IO ()
      bindMat matId = fromMaybe (error "Can't find material to bind") $ Data.Map.lookup matId materialBinds

      folder :: (UUID, IO ()) -> Maybe UUID -> IO (Maybe UUID)
      folder (matId, drawCom) curMatId = do
        when (Just matId /= curMatId) do
          bindMat matId
        drawCom
        pure (Just matId)

  void . liftIO $ foldrM folder Nothing allCommands
  pure a

-- We can use the depth buffer to properly order opaque draws on the
-- screen, so the draws themselves can be sorted by material
-- Blended draws (e.g. for e.g. transparency) are drawn in the order they are submitted
data DrawCommands = DrawCommands
  { opaqueCommands   :: [(UUID, IO ())] -- As they are opaque, we can sort them by material to minimize binds
  , blendedCommands  :: [(UUID, IO ())] -- We can't sort these, but if subsequent commands use the same material, we can avoid multiple binds
  , materialBinds    :: Map UUID (IO ())
  } deriving Generic

newtype CommandT m a = CommandT { unCommandT :: StateT DrawCommands m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState DrawCommands)

instance MonadTrans CommandT where
  lift = CommandT . lift

recordUniform :: (MonadIO m, BatchIOMonad m, FrameMonad m, Storable uniform) => FramedResource (BufferDescriptorSet uniform) -> uniform -> m Word32
recordUniform bds uniform = do
  frameNumber <- frameNumber <$> askFrameContext
  let matDesc = resourceForFrame frameNumber bds
  let uniformRef = queuedData matDesc
  uniformIndex <- liftIO $ fromIntegral . length <$> readIORef uniformRef
  liftIO $ modifyIORef' uniformRef (uniform:)

  recordIO $ uploadBufferDescriptor matDesc

  pure uniformIndex

instance (MonadIO m, FrameMonad m, BatchIOMonad m) => CommandMonad (CommandT m) where
  recordDrawCommand shouldBlend material uniformIndex drawCommand = CommandT do
    FrameContext {..} <- lift askFrameContext
    dcs@DrawCommands {..} <- get

    let matId = view #uuid material
        doDraw = do
          cmdPushMaterialConstants commandBuffer material uniformIndex
          drawCommand commandBuffer
        newMatBinds = Map.insert matId (cmdBindMaterial frameNumber commandBuffer material) materialBinds

    if shouldBlend
    then put $ dcs { materialBinds = newMatBinds, blendedCommands = (matId, doDraw) : blendedCommands }
    else put $ dcs { materialBinds = newMatBinds, opaqueCommands  = (matId, doDraw) : opaqueCommands }

{- Global Descriptor Monad -}

class Monad m => GlobalDescriptorMonad m where
  askDescriptorSet :: m TextureDescriptorSet

useGlobalDecriptorSet :: (FrameMonad m, MonadIO m) => TextureDescriptorSet -> GlobalDescriptorT m a -> m a
useGlobalDecriptorSet tds f = flip runReaderT tds . unGlobalDescriptorT $ do
  f

newtype GlobalDescriptorT m a = GlobalDescriptorT { unGlobalDescriptorT :: ReaderT TextureDescriptorSet m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance Monad m => GlobalDescriptorMonad (GlobalDescriptorT m) where
  askDescriptorSet = GlobalDescriptorT ask

mapGlobalDescriptorT :: (m a -> n b) -> GlobalDescriptorT m a -> GlobalDescriptorT n b
mapGlobalDescriptorT f = GlobalDescriptorT . mapReaderT f . unGlobalDescriptorT

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

-- instance CommandMonad m => CommandMonad (MaterialT material m) where askCommandBuffer = lift askCommandBuffer
instance CommandMonad m => CommandMonad (MatrixT m) where
  recordDrawCommand a b c d = lift $ recordDrawCommand a b c d
instance CommandMonad m => CommandMonad (DynamicMeshT m) where
  recordDrawCommand a b c d = lift $ recordDrawCommand a b c d
instance CommandMonad m => CommandMonad (GlobalDescriptorT m) where
  recordDrawCommand a b c d = lift $ recordDrawCommand a b c d

instance GlobalDescriptorMonad m => GlobalDescriptorMonad (MatrixT m)  where askDescriptorSet = lift askDescriptorSet
instance GlobalDescriptorMonad m => GlobalDescriptorMonad (CommandT m) where askDescriptorSet = lift askDescriptorSet

instance BatchIOMonad m => BatchIOMonad (CommandT m)          where recordIO = lift . recordIO
instance BatchIOMonad m => BatchIOMonad (MatrixT m)           where recordIO = lift . recordIO
instance BatchIOMonad m => BatchIOMonad (DynamicMeshT m)      where recordIO = lift . recordIO
instance BatchIOMonad m => BatchIOMonad (GlobalDescriptorT m) where recordIO = lift . recordIO

instance FrameMonad m => FrameMonad (MatrixT m)           where askFrameContext   = lift askFrameContext
instance FrameMonad m => FrameMonad (CommandT m)          where askFrameContext   = lift askFrameContext
instance FrameMonad m => FrameMonad (GlobalDescriptorT m) where askFrameContext   = lift askFrameContext
instance FrameMonad m => FrameMonad (DynamicMeshT m)      where askFrameContext   = lift askFrameContext
instance FrameMonad m => FrameMonad (BatchIOT m)          where askFrameContext   = lift askFrameContext

instance DynamicMeshMonad m => DynamicMeshMonad (MatrixT m) where
  askDynamicMesh = lift askDynamicMesh
  getMeshes      = lift getMeshes
  addMesh m      = lift $ addMesh m

instance DynamicMeshMonad m => DynamicMeshMonad (GlobalDescriptorT m) where
  askDynamicMesh = lift askDynamicMesh
  getMeshes      = lift getMeshes
  addMesh m      = lift $ addMesh m

instance DynamicMeshMonad m => DynamicMeshMonad (CommandT m) where
  askDynamicMesh = lift askDynamicMesh
  getMeshes      = lift getMeshes
  addMesh m      = lift $ addMesh m

instance MonadReader r m => MonadReader r (CommandT m) where
  ask = lift ask
  local f = CommandT . mapStateT id . unCommandT . local f

instance MonadReader r m => MonadReader r (DynamicMeshT m) where
  ask = lift ask
  local f = mapDynamicMeshT id . local f

instance MonadReader r m => MonadReader r (GlobalDescriptorT m) where
  ask = lift ask
  local f = mapGlobalDescriptorT id . local f

instance MonadReader r m => MonadReader r (FrameT m) where
  ask = lift ask
  local f = mapFrameT id . local f

instance MonadReader r m => MonadReader r (BatchIOT m) where
  ask = lift ask
  local f = BatchIOT . mapWriterT id . unBatchIOT . local f

{- Utilities -}

getTexIdx :: GlobalDescriptorMonad m => Text -> m Word32
getTexIdx name = do
  TextureDescriptorSet {..} <- askDescriptorSet
  pure . fromIntegral $ fromMaybe (error $ "Can't find texture '" ++ unpack name ++ "' in material") $ V.elemIndex name textureNames

packVecs :: (Storable a, Foldable f) => [f a] -> SV.Vector a
packVecs = SV.fromList . concatMap toList

textMesh :: Font Int -> TextCommand -> Mesh
textMesh font tc = Mesh { indices = Just (SV.fromList indices), vertices = [(Position, packVecs posVecs), (TextureCoord, packVecs tcVecs)] }
  where
  (numSquares, posVecs, tcVecs) = transformTextCommandToVerts tc font
  (indices, _numBlockIndices) = squareIndices (fromIntegral numSquares)

drawText
  :: (CommandMonad m, DynamicMeshMonad m, MonadIO m, Storable uniform)
  => BufferedUniformMaterial uniform
  -> uniform
  -> Font Int
  -> TextCommand
  -> PointedDescriptorSet
  -> m ()
drawText material uniform font tc texDescriptorSet = drawDynamicMesh True material uniform (textMesh font tc) (Just texDescriptorSet)

-- |The mesh needs to supply, at a minimum, all the attributes required by the material
drawDynamicMesh
  :: (CommandMonad m, MonadIO m, Storable uniform, DynamicMeshMonad m)
  => Bool
  -> BufferedUniformMaterial uniform
  -> uniform
  -> Mesh
  -> Maybe PointedDescriptorSet
  -> m ()
drawDynamicMesh shouldBlend BufferedUniformMaterial {..} uniform mesh drawBufferDescriptorSet = do
  meshes <- getMeshes
  addMesh mesh

  uniformIndex <- recordUniform descriptor uniform

  -- This is O(n)... Might want to cache this
  let vertexSizeThusFar = sum $ map (sum . map (vsizeOf . snd) . vertices) meshes
      indexSizeThusFar  = sum $ map (maybe 0 vsizeOf . indices) meshes

  DynamicBufferedMesh { vertexBufferPair = (vertexBuffer,_), indexBufferPair = (indexBuffer,_) } <- askDynamicMesh
  recordDrawCommand shouldBlend material uniformIndex \commandBuffer -> do
    for_ drawBufferDescriptorSet $ cmdBindDrawDescriptorSet commandBuffer material
    cmdDrawBufferedMesh commandBuffer material mesh vertexSizeThusFar vertexBuffer (fromIntegral indexSizeThusFar) (Just indexBuffer)

-- |The mesh needs to supply, at a minimum, all the attributes required by the material
drawMesh
  :: (CommandMonad m, MonadIO m, Storable uniform)
  => Bool
  -> BufferedUniformMaterial uniform
  -> uniform
  -> BufferedMesh
  -> Maybe PointedDescriptorSet
  -> m ()
drawMesh shouldBlend BufferedUniformMaterial {..} uniform BufferedMesh {..} drawBufferDescriptorSet = do
  uniformIndex <- recordUniform descriptor uniform
  recordDrawCommand shouldBlend material uniformIndex $ \cb -> do
      for_ drawBufferDescriptorSet $ cmdBindDrawDescriptorSet cb material
      cmdDrawBufferedMesh cb material mesh 0 vertexBuffer 0 indexBuffer

cmdDrawBufferedMesh :: MonadIO m => CommandBuffer -> Material -> Mesh -> Word32 -> Buffer -> Word64 -> Maybe Buffer -> m ()
cmdDrawBufferedMesh commandBuffer Material {..} mesh vertexOffset vertexBuffer indexOffset mIndexBuffer = do
  let meshOffsets = snd $ mapAccumL (\s (a,vec) -> (s + vsizeOf vec, (a, s))) vertexOffset (sortOn (attrLocation . fst) (vertices mesh))
      bindOffsets = V.fromList $ attributes <&> \a -> fromIntegral . fromMaybe (error $ "Can't find attribute '" ++ show a ++ "' in material")
                                                    $ Prelude.lookup a meshOffsets
      buffers = V.fromList $ vertexBuffer <$ attributes

  cmdBindVertexBuffers commandBuffer 0 buffers bindOffsets

  case (indices mesh, mIndexBuffer) of
    (Just is, Just ibuf) -> do
      cmdBindIndexBuffer commandBuffer ibuf indexOffset INDEX_TYPE_UINT32
      cmdDrawIndexed commandBuffer (fromIntegral . SV.length $ is) 1 0 0 0
    (Nothing, Nothing) -> do
      cmdDraw commandBuffer (fromIntegral $ numVerts mesh) 1 0 0
    _ -> error "Mesh has indices but they aren't buffered."
