{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Hickory.Vulkan.Monad where

import Vulkan (CommandBuffer, cmdBindVertexBuffers, cmdBindIndexBuffer, cmdDrawIndexed, IndexType(..), cmdBindDescriptorSets, pattern PIPELINE_BIND_POINT_GRAPHICS)
import Control.Monad.Reader (ReaderT (..), runReaderT, ask, MonadReader, local, mapReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (MonadTrans, lift)
import Hickory.Vulkan.Material (Material (..), cmdPushMaterialConstants, cmdBindMaterial)
import Hickory.Vulkan.DescriptorSet (TextureDescriptorSet (..))
import Hickory.Vulkan.Mesh (cmdDrawBufferedMesh, BufferedMesh)
import Foreign (sizeOf, Storable)
import Hickory.Graphics.MatrixMonad (MatrixT)
import Hickory.Vulkan.Text (DynamicBufferedMesh(..), uploadDynamicMesh)
import Control.Monad.State.Strict (StateT (..), evalStateT, get, put)
import qualified Data.Vector.Storable as SV
import qualified Data.Vector as V
import Data.Word (Word32)
import Control.Monad.State.Class (modify)
import Hickory.Text.Text (TextCommand, Font, PositionedTextCommand (..), transformTextCommandsToVerts)
import Hickory.Graphics.DrawText (squareIndices)
import Linear (V3(..))
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import Data.UUID (UUID)
import Data.Map (Map, lookup, insert)
import Data.Foldable (foldrM)
import Data.List (sortOn)
import Control.Monad (when, void)

{- Command Monad -}

class Monad m => CommandMonad m where
  askCommandBuffer  :: m CommandBuffer
  recordDrawCommand
    :: Storable pushConst
    => Bool -- True if blended
    -> Material pushConst
    -> pushConst
    -> (CommandBuffer -> IO ())
    -> m ()

recordCommandBuffer :: MonadIO m => CommandBuffer -> CommandT m a -> m a
recordCommandBuffer commandBuffer f = flip runReaderT commandBuffer . flip evalStateT (DrawCommands mempty mempty mempty) $ do
  a <- unCommandT f
  DrawCommands {..} <- get
  let allCommands = blendedCommands ++ sortOn fst opaqueCommands
      bindMat :: UUID -> IO ()
      bindMat matId = fromMaybe (error "Can't find material to bind") $ Data.Map.lookup matId materialBinds
  void . liftIO $ foldrM (\(matId, drawCom :: IO ()) curMatId -> when (Just matId /= curMatId) (bindMat matId) >> drawCom >> pure (Just matId)) Nothing allCommands
  pure a

-- We can use the depth buffer to properly order opaque draws on the
-- screen, so the draws themselves can be sorted by material
-- Blended draws (e.g. for e.g. transparency) are drawn in the order they are submitted
data DrawCommands = DrawCommands
  { opaqueCommands  :: [(UUID, IO ())] -- As they are opaque, we can sort them by material to minimize binds
  , blendedCommands :: [(UUID, IO ())] -- We can't sort these, but if subsequent commands use the same material, we can avoid multiple binds
  , materialBinds   :: Map UUID (IO ())
  }

newtype CommandT m a = CommandT { unCommandT :: StateT DrawCommands (ReaderT CommandBuffer m) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance MonadTrans CommandT where
  lift = CommandT . lift . lift

instance Monad m => CommandMonad (CommandT m) where
  askCommandBuffer = CommandT ask
  recordDrawCommand shouldBlend material pushConst drawCommand = CommandT do
    commandBuffer <- ask
    dcs@DrawCommands {..} <- get
    let matId = uuid material
        doDraw = do
          cmdPushMaterialConstants commandBuffer material pushConst
          drawCommand commandBuffer
        newMatBinds = Data.Map.insert matId (cmdBindMaterial commandBuffer material) materialBinds

    if shouldBlend
    then put $ dcs { materialBinds = newMatBinds, blendedCommands = (matId, doDraw) : blendedCommands }
    else put $ dcs { materialBinds = newMatBinds, opaqueCommands  = (matId, doDraw) : opaqueCommands }

mapCommandT :: forall m n a b. (Monad m, Monad n) => (m a -> n b) -> CommandT m a -> CommandT n b
mapCommandT f = CommandT . mapStateT' (mapReaderT f) . unCommandT

drawMesh :: (CommandMonad m, Storable pushConst) => Bool -> Material pushConst -> pushConst -> BufferedMesh -> m ()
drawMesh shouldBlend material pushConst mesh = do
  recordDrawCommand shouldBlend material pushConst $
      flip cmdDrawBufferedMesh mesh

{- Global Descriptor Monad -}

class Monad m => GlobalDescriptorMonad m where
  askDescriptorSet :: m TextureDescriptorSet

-- |Need to supply some material with a pipeline compatible with the descriptor set
useGlobalDecriptorSet :: (CommandMonad m, MonadIO m) => TextureDescriptorSet -> Material pushConst -> GlobalDescriptorT m a -> m a
useGlobalDecriptorSet descriptorSet material f = flip runReaderT descriptorSet . unGlobalDescriptorT $ do
  TextureDescriptorSet {..} <- askDescriptorSet
  commandBuffer <- askCommandBuffer
  cmdBindDescriptorSets commandBuffer PIPELINE_BIND_POINT_GRAPHICS (pipelineLayout material) 0 descriptorSets []
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
  getVectors     :: m (SV.Vector Float, SV.Vector Word32)
  addVectors     :: (SV.Vector Float, SV.Vector Word32) -> m ()

useDynamicMesh :: (CommandMonad m, MonadIO m) => DynamicBufferedMesh -> DynamicMeshT m a -> m a
useDynamicMesh dynamicMesh f = flip runReaderT dynamicMesh . flip evalStateT (mempty, mempty) . unDynamicMeshT $ do
  a <- f
  (floats, indices) <- getVectors
  uploadDynamicMesh dynamicMesh floats indices
  pure a

newtype DynamicMeshT m a = DynamicMeshT { unDynamicMeshT :: StateT (SV.Vector Float, SV.Vector Word32) (ReaderT DynamicBufferedMesh m) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance MonadTrans DynamicMeshT where
  lift = DynamicMeshT . lift . lift

instance Monad m => DynamicMeshMonad (DynamicMeshT m) where
  askDynamicMesh = DynamicMeshT ask
  getVectors = DynamicMeshT get
  addVectors (floats, indices) = DynamicMeshT do
    modify \(floats', indices') -> (floats' SV.++ floats, indices' SV.++ indices)

mapStateT' :: (Functor m, Functor n) => (m a -> n b) -> StateT s m a -> StateT s n b
mapStateT' f m = StateT $ \s -> (,s) <$> f (fst <$> runStateT m s)

mapDynamicMeshT :: forall m n a b. (Monad m, Monad n) => (m a -> n b) -> DynamicMeshT m a -> DynamicMeshT n b
mapDynamicMeshT f = DynamicMeshT . mapStateT' (mapReaderT f) . unDynamicMeshT

{- Transitive Instances -}

-- instance CommandMonad m => CommandMonad (MaterialT material m) where askCommandBuffer = lift askCommandBuffer
instance CommandMonad m => CommandMonad (MatrixT m) where
  askCommandBuffer = lift askCommandBuffer
  recordDrawCommand a b c d = lift $ recordDrawCommand a b c d
instance CommandMonad m => CommandMonad (DynamicMeshT m) where
  askCommandBuffer = lift askCommandBuffer
  recordDrawCommand a b c d = lift $ recordDrawCommand a b c d
instance CommandMonad m => CommandMonad (GlobalDescriptorT m) where
  askCommandBuffer = lift askCommandBuffer
  recordDrawCommand a b c d = lift $ recordDrawCommand a b c d

instance GlobalDescriptorMonad m => GlobalDescriptorMonad (MatrixT m) where askDescriptorSet = lift askDescriptorSet

instance DynamicMeshMonad m => DynamicMeshMonad (MatrixT m) where
  askDynamicMesh = lift askDynamicMesh
  getVectors     = lift getVectors
  addVectors v   = lift $ addVectors v

instance DynamicMeshMonad m => DynamicMeshMonad (GlobalDescriptorT m) where
  askDynamicMesh = lift askDynamicMesh
  getVectors     = lift getVectors
  addVectors v   = lift $ addVectors v


instance MonadReader r m => MonadReader r (CommandT m) where
  ask = lift ask
  local f = mapCommandT id . local f

instance MonadReader r m => MonadReader r (DynamicMeshT m) where
  ask = lift ask
  local f = mapDynamicMeshT id . local f

{- Utilities -}

draw :: (CommandMonad m, MonadIO m) => BufferedMesh -> m ()
draw mesh = do
  commandBuffer <- askCommandBuffer
  cmdDrawBufferedMesh commandBuffer mesh

getTexIdx :: GlobalDescriptorMonad m => Text -> m Word32
getTexIdx name = do
  TextureDescriptorSet {..} <- askDescriptorSet
  pure . fromIntegral $ fromMaybe (error $ "Can't find texture " ++ unpack name ++ " in material") $ V.elemIndex name textureNames

drawText :: (CommandMonad m, DynamicMeshMonad m, MonadIO m, Storable pushConst) => Material pushConst -> pushConst -> Font Int -> TextCommand -> m ()
drawText material pushConst font tc = do
  (oldFloats, oldIndices) <- getVectors
  let (numSquares, floats) = transformTextCommandsToVerts [PositionedTextCommand (V3 0 0 0) tc] font
      (indices, _numBlockIndices) = squareIndices (fromIntegral numSquares)
  addVectors (SV.fromList floats, SV.fromList indices)

  DynamicBufferedMesh { vertexBufferPair = (vertexBuffer,_), indexBufferPair = (indexBuffer,_) } <- askDynamicMesh

  recordDrawCommand True material pushConst \commandBuffer -> do

    -- Offset vertex and index buffers by any data we've stored so far
    cmdBindVertexBuffers commandBuffer 0 [vertexBuffer] [fromIntegral $ SV.length oldFloats  * sizeOf (undefined :: Float)]
    cmdBindIndexBuffer   commandBuffer    indexBuffer   (fromIntegral $ SV.length oldIndices * sizeOf (undefined :: Word32)) INDEX_TYPE_UINT32

    cmdDrawIndexed commandBuffer (fromIntegral . length $ indices) 1 0 0 0
