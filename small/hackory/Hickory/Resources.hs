{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hickory.Resources where

import qualified Data.HashMap.Strict as Map
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.IORef (IORef, readIORef, newIORef, modifyIORef', atomicModifyIORef')
import Acquire (Acquire)
import Hickory.Vulkan.Vulkan (unWrapAcquire, mkAcquire)
import Control.Monad (unless, join, (<=<))
import Control.Lens (view, (^.))
import Hickory.Vulkan.Text (TextRenderer, withTextRenderer)
import Vulkan (Filter (..), SamplerAddressMode (..), SamplerMipmapMode)
import Hickory.Vulkan.Types (PointedDescriptorSet (..), BufferedMesh (..), VulkanResources, addCleanup, TextureLoadOptions)
import GHC.Generics (Generic)
import Hickory.Vulkan.Mesh (withBufferedMesh, loadMeshFromFile)
import Hickory.Vulkan.DescriptorSet (withTextureDescriptorSet, withDescriptorSet)
import System.FilePath.Lens (extension, filename, basename)
import Control.Monad.Reader.Class (ask, MonadReader (..))
import Data.Maybe (fromMaybe, listToMaybe)
import System.Directory (doesFileExist)
import Hickory.Vulkan.StockMesh (withCubeMesh, withSquareMesh)
import Hickory.Vulkan.StockTexture (withWhiteImageDescriptor)
import Linear (M44)
import qualified Data.Vector as V
import Control.Monad.Reader (ReaderT (..), MonadTrans, mapReaderT, lift)
import Control.Monad.Writer.Class (MonadWriter (..))
import Data.Dynamic (Dynamic, Typeable, fromDynamic, toDyn)
import GHC.Compact (compact, getCompact)

type ResourceStore a = IORef (Map.HashMap String (a, IO ()))

newResourceStore :: IO (ResourceStore a)
newResourceStore = newIORef mempty

cleanupStore :: ResourceStore a -> IO ()
cleanupStore ref = do
  m <- readIORef ref
  sequence_ $ snd . snd <$> Map.toList m

loadResource :: ResourceStore a -> String -> Acquire (Maybe a) -> IO (IO ())
loadResource ref k f = do
  unWrapAcquire f >>= \case
    (Just res, cleanup) -> do
      comp <- getCompact <$> compact res
      atomicModifyIORef' ref \m -> case Map.lookup k m of
        Just (_, cleanupOld) -> (Map.insert k (comp, cleanup) m, cleanupOld)
        Nothing              -> (Map.insert k (comp, cleanup) m, pure ())
    (Nothing, cleanup) -> modifyIORef' ref (Map.delete k) >> cleanup >> pure (pure ())

loadResourceNoReplace :: ResourceStore a -> String -> Acquire (Maybe a) -> IO ()
loadResourceNoReplace ref k f = do
  temp <- readIORef ref
  unless (Map.member k temp) do
    join $ unWrapAcquire f >>= \case
      (Just res, cleanup) -> do
        comp <- getCompact <$> compact res
        atomicModifyIORef' ref \m -> case Map.lookup k m of
          Just (_, _cleanupOld) -> error "Resource map should not have resource"
          Nothing               -> (Map.insert k (comp, cleanup) m, pure ())
      (Nothing, cleanup) -> pure cleanup

loadResource' :: ResourceStore a -> String -> Acquire a -> IO (IO ())
loadResource' ref k f = loadResource ref k (Just <$> f)

getResources :: ResourceStore a -> IO (Map.HashMap String a)
getResources = fmap (Map.map fst) . readIORef

data ResourcesStore = ResourcesStore
  { meshes       :: ResourceStore BufferedMesh
  , textures     :: ResourceStore PointedDescriptorSet
  , fonts        :: ResourceStore TextRenderer
  , skins        :: ResourceStore (Map.HashMap String (V.Vector (V.Vector (M44 Float))))
  , untyped      :: ResourceStore Dynamic
  } deriving (Generic)

data Resources = Resources
  { meshes     :: Map.HashMap String BufferedMesh
  , textures   :: Map.HashMap String PointedDescriptorSet
  , fonts      :: Map.HashMap String TextRenderer
  , skins      :: Map.HashMap String (Map.HashMap String (V.Vector (V.Vector (M44 Float))))
  , untyped    :: Map.HashMap String Dynamic
  } deriving (Generic)

-- instance Hashable SamplerAddressMode where hashWithSalt s (SamplerAddressMode i) = hashWithSalt s i
-- instance Hashable Filter where hashWithSalt s (Filter i) = hashWithSalt s i

loadTextureResource :: VulkanResources -> ResourcesStore -> String -> TextureLoadOptions -> IO ()
loadTextureResource vulkanResources ResourcesStore { textures } path textureLoadOptions =
  loadResourceNoReplace textures (path ^. filename) do
    liftIO (doesFileExist path) >>= \case
      True -> fmap Just $ view #descriptorSet <$> withTextureDescriptorSet vulkanResources [(path, textureLoadOptions)]
      False -> pure Nothing

loadMeshResource :: VulkanResources -> ResourcesStore -> String -> IO ()
loadMeshResource vulkanResources ResourcesStore { meshes } path = do
  loadResourceNoReplace meshes (path ^. filename) do
    liftIO (doesFileExist path) >>= \case
      True ->
        case path ^. extension of
          ".hmdl" -> fmap Just $ liftIO (loadMeshFromFile path) >>= withBufferedMesh vulkanResources (Just $ path ^. filename)
          _       -> pure Nothing
      False -> pure Nothing

loadFontResource :: VulkanResources -> ResourcesStore -> String -> String -> Float -> IO ()
loadFontResource vulkanResources ResourcesStore { fonts } fontPath imagePath msdfDist = do
  loadResourceNoReplace fonts (fontPath ^. basename) do
    liftIO ((&&) <$> doesFileExist fontPath <*> doesFileExist imagePath) >>= \case
      True -> Just <$> withTextRenderer vulkanResources fontPath imagePath msdfDist
      False -> pure Nothing

loadUntypedResource :: Typeable a => VulkanResources -> ResourcesStore -> String -> Acquire (Maybe a) -> IO ()
loadUntypedResource vulkanResources ResourcesStore { untyped } path
  = addCleanup vulkanResources <=< loadResource untyped path . fmap (fmap toDyn)

withResourcesStore :: VulkanResources -> Acquire ResourcesStore
withResourcesStore vulkanResources = do
  textures   <- liftIO newResourceStore
  meshes     <- liftIO newResourceStore
  fonts      <- liftIO newResourceStore
  skins      <- liftIO newResourceStore
  untyped    <- liftIO newResourceStore

  liftIO do
    join $ loadResource' textures "white" do
      des <- withWhiteImageDescriptor vulkanResources
      withDescriptorSet vulkanResources [des]
    join $ loadResource' meshes "square" (withSquareMesh vulkanResources)
    join $ loadResource' meshes "cube" (withCubeMesh vulkanResources)

  mkAcquire (pure ()) (const $ cleanupStore meshes)
  mkAcquire (pure ()) (const $ cleanupStore textures)
  mkAcquire (pure ()) (const $ cleanupStore fonts)
  mkAcquire (pure ()) (const $ cleanupStore skins)
  mkAcquire (pure ()) (const $ cleanupStore untyped)

  pure ResourcesStore {..}

getResourcesStoreResources :: ResourcesStore -> IO Resources
getResourcesStoreResources ResourcesStore {..} =
  Resources <$> getResources meshes
            <*> getResources textures
            <*> getResources fonts
            <*> getResources skins
            <*> getResources untyped

getMesh :: ResourcesMonad m => String -> m BufferedMesh
getMesh name = fromMaybe (error $ "Can't find mesh: " ++ name) <$> getMeshMay name

getMeshMay :: ResourcesMonad m => String -> m (Maybe BufferedMesh)
getMeshMay name = do
  ms <- view #meshes <$> askResources
  pure $ Map.lookup name ms


getTexture :: (ResourcesMonad m) => String -> m PointedDescriptorSet
getTexture name = fromMaybe (error $ "Can't find texture: " ++ name) <$> getTextureMay name

getTextureMay :: ResourcesMonad m => String -> m (Maybe PointedDescriptorSet)
getTextureMay name = do
  ts <- view #textures <$> askResources
  pure $ Map.lookup name ts

getFont :: ResourcesMonad m => String -> m TextRenderer
getFont name = fromMaybe (error $ "Can't find font: " ++ name) <$> getFontMay name

getFontMay :: ResourcesMonad m => String -> m (Maybe TextRenderer)
getFontMay name = do
  ms <- view #fonts <$> askResources
  pure $ Map.lookup name ms

getSomeFont :: ResourcesMonad m => m TextRenderer
getSomeFont = do
  fs <- view #fonts <$> askResources
  pure $ fromMaybe (error "No font loaded") . listToMaybe . Map.elems $ fs

getSkin :: ResourcesMonad m => String -> m (Map.HashMap String (V.Vector (V.Vector (M44 Float))))
getSkin name = fromMaybe (error $ "Can't find skin: " ++ name) <$> getSkinMay name

getSkinMay :: ResourcesMonad m => String -> m (Maybe (Map.HashMap String (V.Vector (V.Vector (M44 Float)))))
getSkinMay name = do
  ms <- view #skins <$> askResources
  pure $ Map.lookup name ms

getUntyped :: (Typeable a, ResourcesMonad m) => String -> m a
getUntyped name = fromMaybe (error $ "Can't find resource: " ++ name) <$> getUntypedMay name

getUntypedMay :: (Typeable a, ResourcesMonad m) => String -> m (Maybe a)
getUntypedMay name = do
  ms <- view #untyped <$> askResources
  pure $ fromDynamic =<< Map.lookup name ms

class Monad m => ResourcesMonad m where
  askResources  :: m Resources

newtype ResourcesT m a = ResourcesT { unResourcesT :: ReaderT Resources m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance Monad m => ResourcesMonad (ResourcesT m) where
  askResources   = ResourcesT ask

runResources :: Resources -> ResourcesT m a -> m a
runResources fc = flip runReaderT fc . unResourcesT

mapResourcesT :: (m a -> n b) -> ResourcesT m a -> ResourcesT n b
mapResourcesT f = ResourcesT . mapReaderT f . unResourcesT

instance {-# OVERLAPPABLE #-} (MonadTrans t, ResourcesMonad m, Monad (t m)) => ResourcesMonad (t m) where askResources = lift askResources

instance MonadReader r m => MonadReader r (ResourcesT m) where
  ask = lift ask
  local f = mapResourcesT id . local f

instance MonadWriter r m => MonadWriter r (ResourcesT m) where
  tell = lift . tell
  listen = mapResourcesT listen
  pass = mapResourcesT pass
