{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Hickory.Vulkan.Monad where

import Vulkan (CommandBuffer)
import Control.Monad.Reader (ReaderT, runReaderT, ask, MonadReader, local, mapReaderT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (MonadTrans, lift)
import Hickory.Vulkan.Material (Material, cmdPushMaterialConstants, cmdBindMaterial)
import Hickory.Vulkan.Mesh (cmdDrawBufferedMesh, BufferedMesh)
import Foreign (Storable)
import Hickory.Graphics.MatrixMonad (MatrixT, MatrixMonad, xform, askMatrix)

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


instance CommandMonad m => CommandMonad (MaterialT material m) where askCommandBuffer = lift askCommandBuffer
instance CommandMonad m => CommandMonad (MatrixT m) where askCommandBuffer = lift askCommandBuffer
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

pushConstant :: (CommandMonad m, MaterialMonad pushConstant m, MonadIO m, Storable pushConstant) => pushConstant -> m ()
pushConstant constant = do
  commandBuffer <- askCommandBuffer
  material      <- askMaterial
  cmdPushMaterialConstants commandBuffer material constant

draw :: (CommandMonad m, MonadIO m) => BufferedMesh -> m ()
draw mesh = do
  commandBuffer <- askCommandBuffer
  cmdDrawBufferedMesh commandBuffer mesh
