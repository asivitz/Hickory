{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Hickory.Vulkan.Monad where

import GHC.Generics (Generic)
import Control.Lens ((%=), view)
import Control.Monad (when, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT (..), runReaderT, ask, MonadReader, local, mapReaderT)
import Control.Monad.State.Class (modify, MonadState)
import Control.Monad.State.Strict (StateT (..), evalStateT, get, put)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Foldable (foldrM, toList)
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
import Hickory.Vulkan.DescriptorSet (PointedDescriptorSet (..), TextureDescriptorSet (..), BufferDescriptorSet(..), uploadBufferDescriptor, withBufferDescriptorSet, descriptorSetBinding, PointedDescriptorSet)
import Hickory.Vulkan.Frame (FrameContext (..))
import Hickory.Vulkan.Framing (resourceForFrame, FramedResource, frameResource)
import Hickory.Vulkan.Material (Material (..), cmdPushMaterialConstants, cmdBindMaterial, withMaterial)
import Hickory.Vulkan.Mesh (BufferedMesh (..), vsizeOf, attrLocation, Mesh(..), numVerts, Attribute(..))
import Hickory.Vulkan.Text (DynamicBufferedMesh(..), uploadDynamicMesh)
import Vulkan (CommandBuffer, cmdBindVertexBuffers, cmdDraw, cmdBindIndexBuffer, cmdDrawIndexed, IndexType(..), cmdBindDescriptorSets, pattern PIPELINE_BIND_POINT_GRAPHICS, Buffer, PrimitiveTopology(..))
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Hickory.Vulkan.Vulkan (VulkanResources, Swapchain)
import Control.Monad.Managed (Managed)
import qualified Data.ByteString as B


data BufferedUniformMaterial uniform = BufferedUniformMaterial
  { material   :: Material
  , descriptor :: FramedResource (BufferDescriptorSet uniform)
  }

withBufferedUniformMaterial
  :: Storable uniform
  => VulkanResources
  -> Swapchain
  -> [Attribute]
  -> B.ByteString
  -> B.ByteString
  -> Maybe PointedDescriptorSet
  -> Managed (BufferedUniformMaterial uniform)
withBufferedUniformMaterial vulkanResources swapchain attributes vert frag globalDescriptor = do
  descriptor <- frameResource $ withBufferDescriptorSet vulkanResources
  material <- withMaterial vulkanResources swapchain attributes PRIMITIVE_TOPOLOGY_TRIANGLE_LIST vert frag (view #descriptorSet <$> descriptor) globalDescriptor
  pure BufferedUniformMaterial {..}

{- Command Monad -}

class Monad m => CommandMonad m where
  askCommandBuffer  :: m CommandBuffer
  askFrameNumber    :: m Int
  recordDrawCommand
    :: Bool -- True if blended
    -> Material
    -> Word32
    -> (CommandBuffer -> IO ())
    -> m ()
  recordFinalIO :: IO () -> m ()

recordCommandBuffer :: MonadIO m => FrameContext -> CommandT m a -> m a
recordCommandBuffer FrameContext {..} f = flip runReaderT (frameNumber, commandBuffer) . flip evalStateT (DrawCommands mempty mempty mempty (pure ())) $ do
  a <- unCommandT f
  DrawCommands {..} <- get

  liftIO extraIO

  let allCommands = blendedCommands ++ sortOn fst opaqueCommands
      bindMat :: UUID -> IO ()
      bindMat matId = fromMaybe (error "Can't find material to bind") $ Data.Map.lookup matId materialBinds
  void . liftIO $ foldrM (\(matId, drawCom :: IO ()) curMatId -> when (Just matId /= curMatId) (bindMat matId) >> drawCom >> pure (Just matId)) Nothing allCommands
  pure a

-- We can use the depth buffer to properly order opaque draws on the
-- screen, so the draws themselves can be sorted by material
-- Blended draws (e.g. for e.g. transparency) are drawn in the order they are submitted
data DrawCommands = DrawCommands
  { opaqueCommands   :: [(UUID, IO ())] -- As they are opaque, we can sort them by material to minimize binds
  , blendedCommands  :: [(UUID, IO ())] -- We can't sort these, but if subsequent commands use the same material, we can avoid multiple binds
  , materialBinds    :: Map UUID (IO ())
  , extraIO          :: IO ()
  } deriving Generic

newtype CommandT m a = CommandT { unCommandT :: StateT DrawCommands (ReaderT (Int, CommandBuffer) m) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState DrawCommands)

instance MonadTrans CommandT where
  lift = CommandT . lift . lift

recordUniform :: (MonadIO m, CommandMonad m, Storable uniform) => FramedResource (BufferDescriptorSet uniform) -> uniform -> m Word32
recordUniform bds uniform = do
  frameNumber <- askFrameNumber
  let matDesc = resourceForFrame frameNumber bds
  let uniformRef = queuedData matDesc
  uniformIndex <- liftIO $ fromIntegral . length <$> readIORef uniformRef
  liftIO $ modifyIORef' uniformRef (uniform:)

  recordFinalIO $ uploadBufferDescriptor matDesc

  pure uniformIndex

instance MonadIO m => CommandMonad (CommandT m) where
  askCommandBuffer = snd <$> CommandT ask
  askFrameNumber   = fst <$> CommandT ask
  recordDrawCommand shouldBlend material uniformIndex drawCommand = CommandT do
    (frameNumber, commandBuffer) <- ask
    dcs@DrawCommands {..} <- get

    let matId = uuid material
        doDraw = do
          cmdPushMaterialConstants commandBuffer material uniformIndex
          drawCommand commandBuffer
        newMatBinds = Map.insert matId (cmdBindMaterial frameNumber commandBuffer material) materialBinds

    if shouldBlend
    then put $ dcs { materialBinds = newMatBinds, blendedCommands = (matId, doDraw) : blendedCommands }
    else put $ dcs { materialBinds = newMatBinds, opaqueCommands  = (matId, doDraw) : opaqueCommands }
  recordFinalIO f = CommandT do
    #extraIO %= (>> f)

-- storableConvert :: (Storable a, Storable b) -> a -> IO b
-- storableConvert

mapCommandT :: forall m n a b. (Monad m, Monad n) => (m a -> n b) -> CommandT m a -> CommandT n b
mapCommandT f = CommandT . mapStateT' (mapReaderT f) . unCommandT

{- Global Descriptor Monad -}

class Monad m => GlobalDescriptorMonad m where
  askDescriptorSet :: m TextureDescriptorSet

-- |Need to supply some material with a pipeline compatible with the descriptor set
useGlobalDecriptorSet :: (CommandMonad m, MonadIO m) => TextureDescriptorSet -> Material -> GlobalDescriptorT m a -> m a
useGlobalDecriptorSet tds material f = flip runReaderT tds . unGlobalDescriptorT $ do
  TextureDescriptorSet {..} <- askDescriptorSet
  commandBuffer <- askCommandBuffer
  cmdBindDescriptorSets commandBuffer PIPELINE_BIND_POINT_GRAPHICS (pipelineLayout material) 0 (descriptorSets descriptorSet) []
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

useDynamicMesh :: (CommandMonad m, MonadIO m) => DynamicBufferedMesh -> DynamicMeshT m a -> m a
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
  askCommandBuffer = lift askCommandBuffer
  askFrameNumber   = lift askFrameNumber
  recordFinalIO    = lift . recordFinalIO
  recordDrawCommand a b c d = lift $ recordDrawCommand a b c d
instance CommandMonad m => CommandMonad (DynamicMeshT m) where
  askCommandBuffer = lift askCommandBuffer
  askFrameNumber   = lift askFrameNumber
  recordFinalIO    = lift . recordFinalIO
  recordDrawCommand a b c d = lift $ recordDrawCommand a b c d
instance CommandMonad m => CommandMonad (GlobalDescriptorT m) where
  askCommandBuffer = lift askCommandBuffer
  askFrameNumber   = lift askFrameNumber
  recordDrawCommand a b c d = lift $ recordDrawCommand a b c d
  recordFinalIO    = lift . recordFinalIO

instance GlobalDescriptorMonad m => GlobalDescriptorMonad (MatrixT m) where askDescriptorSet = lift askDescriptorSet

instance DynamicMeshMonad m => DynamicMeshMonad (MatrixT m) where
  askDynamicMesh = lift askDynamicMesh
  getMeshes      = lift getMeshes
  addMesh m      = lift $ addMesh m

instance DynamicMeshMonad m => DynamicMeshMonad (GlobalDescriptorT m) where
  askDynamicMesh = lift askDynamicMesh
  getMeshes      = lift getMeshes
  addMesh m      = lift $ addMesh m


instance MonadReader r m => MonadReader r (CommandT m) where
  ask = lift ask
  local f = mapCommandT id . local f

instance MonadReader r m => MonadReader r (DynamicMeshT m) where
  ask = lift ask
  local f = mapDynamicMeshT id . local f

instance MonadReader r m => MonadReader r (GlobalDescriptorT m) where
  ask = lift ask
  local f = mapGlobalDescriptorT id . local f

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
  -> m ()
drawText material uniform font tc = drawDynamicMesh True material uniform (textMesh font tc)

-- |The mesh needs to supply, at a minimum, all the attributes required by the material
drawDynamicMesh
  :: (CommandMonad m, MonadIO m, Storable uniform, DynamicMeshMonad m)
  => Bool
  -> BufferedUniformMaterial uniform
  -> uniform
  -> Mesh
  -> m ()
drawDynamicMesh shouldBlend BufferedUniformMaterial {..} uniform mesh = do
  meshes <- getMeshes
  addMesh mesh

  uniformIndex <- recordUniform descriptor uniform

  -- This is O(n)... Might want to cache this
  let vertexSizeThusFar = sum $ map (sum . map (vsizeOf . snd) . vertices) meshes
      indexSizeThusFar  = sum $ map (maybe 0 vsizeOf . indices) meshes

  DynamicBufferedMesh { vertexBufferPair = (vertexBuffer,_), indexBufferPair = (indexBuffer,_) } <- askDynamicMesh
  recordDrawCommand shouldBlend material uniformIndex \commandBuffer -> do
    cmdDrawBufferedMesh commandBuffer material mesh vertexSizeThusFar vertexBuffer (fromIntegral indexSizeThusFar) (Just indexBuffer)

-- |The mesh needs to supply, at a minimum, all the attributes required by the material
drawMesh
  :: (CommandMonad m, MonadIO m, Storable uniform)
  => Bool
  -> BufferedUniformMaterial uniform
  -> uniform
  -> BufferedMesh
  -> m ()
drawMesh shouldBlend BufferedUniformMaterial {..} uniform BufferedMesh {..} = do
  uniformIndex <- recordUniform descriptor uniform
  recordDrawCommand shouldBlend material uniformIndex $ \cb ->
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
