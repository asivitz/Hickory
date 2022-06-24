{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Hickory.Vulkan.Monad where

import Vulkan (CommandBuffer, cmdBindVertexBuffers, cmdBindIndexBuffer, cmdDrawIndexed, IndexType(..))
import Control.Monad.Reader (ReaderT (..), runReaderT, ask, MonadReader, local, mapReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (MonadTrans, lift)
import Hickory.Vulkan.Material (Material, cmdPushMaterialConstants, cmdBindMaterial)
import Hickory.Vulkan.Mesh (cmdDrawBufferedMesh, BufferedMesh)
import Foreign (Storable, sizeOf)
import Hickory.Graphics.MatrixMonad (MatrixT, MatrixMonad, xform, askMatrix)
import Hickory.Vulkan.Text (DynamicBufferedMesh(..), uploadDynamicMesh)
import Control.Monad.State.Strict (StateT (..), evalStateT, get)
import qualified Data.Vector.Storable as SV
import Data.Word (Word32)
import Control.Monad.State.Class (modify)
import Hickory.Text.Text (TextCommand, Font, PositionedTextCommand (..), transformTextCommandsToVerts)
import Hickory.Graphics.DrawText (squareIndices)
import Linear (V3(..))

{- Command Monad -}

class Monad m => CommandMonad m where
  askCommandBuffer :: m CommandBuffer

targetCommandBuffer :: Monad m => CommandBuffer -> CommandT m a -> m a
targetCommandBuffer commandBuffer = flip runReaderT commandBuffer . unCommandT

newtype CommandT m a = CommandT { unCommandT :: ReaderT CommandBuffer m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance Monad m => CommandMonad (CommandT m) where
  askCommandBuffer = CommandT ask

mapCommandT :: (m a -> n b) -> CommandT m a -> CommandT n b
mapCommandT f = CommandT . mapReaderT f . unCommandT

{- Material Monad -}

class Monad m => MaterialMonad material m where
  askMaterial :: m (Material material)

useMaterial :: (CommandMonad m, MonadIO m) => Material material -> MaterialT material m a -> m a
useMaterial material f = flip runReaderT material . unMaterialT $ do
  commandBuffer <- lift askCommandBuffer
  cmdBindMaterial commandBuffer material
  f

newtype MaterialT material m a = MaterialT { unMaterialT :: ReaderT (Material material) m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance Monad m => MaterialMonad material (MaterialT material m) where
  askMaterial = MaterialT ask

mapMaterialT :: (m a -> n b) -> MaterialT material m a -> MaterialT material n b
mapMaterialT f = MaterialT . mapReaderT f . unMaterialT


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

instance CommandMonad m => CommandMonad (MaterialT material m) where askCommandBuffer = lift askCommandBuffer
instance CommandMonad m => CommandMonad (MatrixT m) where askCommandBuffer = lift askCommandBuffer
instance CommandMonad m => CommandMonad (DynamicMeshT m) where askCommandBuffer = lift askCommandBuffer

instance DynamicMeshMonad m => DynamicMeshMonad (MaterialT mat m) where
  askDynamicMesh = lift askDynamicMesh
  getVectors     = lift getVectors
  addVectors v   = lift $ addVectors v

instance DynamicMeshMonad m => DynamicMeshMonad (MatrixT m) where
  askDynamicMesh = lift askDynamicMesh
  getVectors     = lift getVectors
  addVectors v   = lift $ addVectors v

instance MaterialMonad material m => MaterialMonad material (MatrixT m) where askMaterial = lift askMaterial
instance MatrixMonad m => MatrixMonad (MaterialT material m) where
  xform trans = mapMaterialT (xform trans)
  askMatrix = lift askMatrix

instance MonadReader r m => MonadReader r (CommandT m) where
  ask = lift ask
  local f = mapCommandT id . local f

instance MonadReader r m => MonadReader r (MaterialT material m) where
  ask = lift ask
  local f = mapMaterialT id . local f

instance MonadReader r m => MonadReader r (DynamicMeshT m) where
  ask = lift ask
  local f = mapDynamicMeshT id . local f

{- Utilities -}

pushConstant :: (CommandMonad m, MaterialMonad pushConstant m, MonadIO m, Storable pushConstant) => pushConstant -> m ()
pushConstant constant = do
  commandBuffer <- askCommandBuffer
  material      <- askMaterial
  cmdPushMaterialConstants commandBuffer material constant

draw :: (CommandMonad m, MonadIO m) => BufferedMesh -> m ()
draw mesh = do
  commandBuffer <- askCommandBuffer
  cmdDrawBufferedMesh commandBuffer mesh

drawText :: (CommandMonad m, DynamicMeshMonad m, MonadIO m) => Font Int -> TextCommand -> m ()
drawText font tc = do
  (oldFloats, oldIndices) <- getVectors
  let (numSquares, floats) = transformTextCommandsToVerts [PositionedTextCommand (V3 0 0 0) tc] font
      (indices, _numBlockIndices) = squareIndices (fromIntegral numSquares)
  addVectors (SV.fromList floats, SV.fromList indices)

  DynamicBufferedMesh { vertexBufferPair = (vertexBuffer,_), indexBufferPair = (indexBuffer,_) } <- askDynamicMesh

  commandBuffer <- askCommandBuffer

  -- Offset vertex and index buffers by any data we've stored so far
  cmdBindVertexBuffers commandBuffer 0 [vertexBuffer] [fromIntegral $ SV.length oldFloats  * sizeOf (undefined :: Float)]
  cmdBindIndexBuffer   commandBuffer    indexBuffer   (fromIntegral $ SV.length oldIndices * sizeOf (undefined :: Word32)) INDEX_TYPE_UINT32

  cmdDrawIndexed commandBuffer (fromIntegral . length $ indices) 1 0 0 0
