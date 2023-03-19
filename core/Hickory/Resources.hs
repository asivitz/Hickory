{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}

module Hickory.Resources where

import qualified Data.HashMap.Strict as Map
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, readIORef, newIORef, modifyIORef')
import Acquire.Acquire (Acquire)
import Hickory.Vulkan.Vulkan (unWrapAcquire, mkAcquire)
import Control.Monad (unless)
import Control.Lens (view, (^.))
import Hickory.ModelLoading.DirectXModel (ThreeDModel(..), loadModelFromX)
import Hickory.Vulkan.Text (TextRenderer, withTextRenderer)
import Vulkan (Filter (..), SamplerAddressMode (..))
import Hickory.Vulkan.Types (PointedDescriptorSet (..), BufferedMesh (..), VulkanResources)
import GHC.Generics (Generic)
import Hickory.Vulkan.Mesh (withBufferedMesh, loadMeshFromFile)
import Hickory.Vulkan.DescriptorSet (withTextureDescriptorSet, withDescriptorSet)
import System.FilePath.Lens (extension, filename, basename)
import Control.Monad.Reader.Class (MonadReader)
import Data.Maybe (fromMaybe, listToMaybe)
import System.Directory (doesFileExist)
import Hickory.Vulkan.StockMesh (withCubeMesh, withSquareMesh)
import Hickory.Vulkan.StockTexture (withWhiteImageDescriptor)
import Linear (M44)
import qualified Data.Vector as V

type ResourceStore a = IORef (Map.HashMap String (a, IO ()))

newResourceStore :: IO (ResourceStore a)
newResourceStore = newIORef mempty

cleanupStore :: ResourceStore a -> IO ()
cleanupStore ref = do
  m <- readIORef ref
  sequence_ $ snd . snd <$> Map.toList m

loadResource :: ResourceStore a -> String -> Acquire (Maybe a) -> IO ()
loadResource ref k f = do
  m <- readIORef ref
  unless (Map.member k m) do
    unWrapAcquire f >>= \case
      (Just res, cleanup) -> liftIO $ modifyIORef' ref $ Map.insert k (res, cleanup)
      (Nothing, cleanup) -> cleanup

loadResource' :: ResourceStore a -> String -> Acquire a -> IO ()
loadResource' ref k f = do
  res <- unWrapAcquire f
  liftIO $ modifyIORef' ref $ Map.insert k res

getResources :: ResourceStore a -> IO (Map.HashMap String a)
getResources = fmap (Map.map fst) . readIORef

data ResourcesStore = ResourcesStore
  { meshes     :: ResourceStore BufferedMesh
  , textures   :: ResourceStore PointedDescriptorSet
  , fonts      :: ResourceStore TextRenderer
  , animations :: ResourceStore ThreeDModel
  , skins      :: ResourceStore (Map.HashMap String (V.Vector (V.Vector (M44 Float))))
  } deriving (Generic)

data Resources = Resources
  { meshes     :: Map.HashMap String BufferedMesh
  , textures   :: Map.HashMap String PointedDescriptorSet
  , fonts      :: Map.HashMap String TextRenderer
  , animations :: Map.HashMap String ThreeDModel
  , skins      :: Map.HashMap String (Map.HashMap String (V.Vector (V.Vector (M44 Float))))
  } deriving (Generic)

-- instance Hashable SamplerAddressMode where hashWithSalt s (SamplerAddressMode i) = hashWithSalt s i
-- instance Hashable Filter where hashWithSalt s (Filter i) = hashWithSalt s i

loadTextureResource :: VulkanResources -> ResourcesStore -> String -> (Filter, SamplerAddressMode) -> IO ()
loadTextureResource vulkanResources ResourcesStore { textures } path (imageFilter, addressMode) =
  loadResource textures (path ^. filename) do
    liftIO (doesFileExist path) >>= \case
      True -> fmap Just $ view #descriptorSet <$> withTextureDescriptorSet vulkanResources [(path, imageFilter, addressMode)]
      False -> pure Nothing

loadMeshResource :: VulkanResources -> ResourcesStore -> String -> IO ()
loadMeshResource vulkanResources ResourcesStore { meshes } path =
  loadResource meshes (path ^. filename) do
    liftIO (doesFileExist path) >>= \case
      True ->
        case path ^. extension of
          ".hmdl" -> fmap Just $ liftIO (loadMeshFromFile path) >>= withBufferedMesh vulkanResources
          ".x"    -> fmap Just $ liftIO (loadModelFromX   path) >>= withBufferedMesh vulkanResources . packedMesh
          _       -> pure Nothing
      False -> pure Nothing

loadFontResource :: VulkanResources -> ResourcesStore -> String -> String -> Float -> IO ()
loadFontResource vulkanResources ResourcesStore { fonts } fontPath imagePath msdfDist = do
  loadResource fonts (fontPath ^. basename) do
    liftIO ((&&) <$> doesFileExist fontPath <*> doesFileExist imagePath) >>= \case
      True -> Just <$> withTextRenderer vulkanResources fontPath imagePath msdfDist
      False -> pure Nothing

loadAnimationResource :: VulkanResources -> ResourcesStore -> String -> IO ()
loadAnimationResource _vulkanResources ResourcesStore { animations } path =
  loadResource animations (path ^. filename) do
    liftIO $ doesFileExist path >>= \case
      True -> Just <$> loadModelFromX path
      False -> pure Nothing

withResourcesStore :: VulkanResources -> Acquire ResourcesStore
withResourcesStore vulkanResources = do
  textures   <- liftIO newResourceStore
  meshes     <- liftIO newResourceStore
  fonts      <- liftIO newResourceStore
  animations <- liftIO newResourceStore
  skins      <- liftIO newResourceStore

  liftIO do
    loadResource' textures "white" do
      des <- withWhiteImageDescriptor vulkanResources
      withDescriptorSet vulkanResources [des]
    loadResource' meshes "square" (withSquareMesh vulkanResources)
    loadResource' meshes "cube" (withCubeMesh vulkanResources)


  mkAcquire (pure ()) (const $ cleanupStore meshes)
  mkAcquire (pure ()) (const $ cleanupStore textures)
  mkAcquire (pure ()) (const $ cleanupStore fonts)
  mkAcquire (pure ()) (const $ cleanupStore animations)
  mkAcquire (pure ()) (const $ cleanupStore skins)

  pure ResourcesStore {..}

getResourcesStoreResources :: ResourcesStore -> IO Resources
getResourcesStoreResources ResourcesStore {..} =
  Resources <$> getResources meshes
            <*> getResources textures
            <*> getResources fonts
            <*> getResources animations
            <*> getResources skins

getMesh :: MonadReader Resources m => String -> m BufferedMesh
getMesh name = fromMaybe (error $ "Can't find mesh: " ++ name) <$> getMeshMay name

getMeshMay :: MonadReader Resources m => String -> m (Maybe BufferedMesh)
getMeshMay name = do
  ms <- view #meshes
  pure $ Map.lookup name ms


getTexture :: MonadReader Resources m => String -> m PointedDescriptorSet
getTexture name = fromMaybe (error $ "Can't find texture: " ++ name) <$> getTextureMay name

getTextureMay :: MonadReader Resources m => String -> m (Maybe PointedDescriptorSet)
getTextureMay name = do
  ts <- view #textures
  pure $ Map.lookup name ts

getFont :: MonadReader Resources m => String -> m TextRenderer
getFont name = fromMaybe (error $ "Can't find font: " ++ name) <$> getFontMay name

getFontMay :: MonadReader Resources m => String -> m (Maybe TextRenderer)
getFontMay name = do
  ms <- view #fonts
  pure $ Map.lookup name ms

getSomeFont :: MonadReader Resources m => m TextRenderer
getSomeFont = do
  fs <- view #fonts
  pure $ fromMaybe (error "No font loaded") . listToMaybe . Map.elems $ fs

getAnimation :: MonadReader Resources m => String -> m ThreeDModel
getAnimation name = fromMaybe (error $ "Can't find animation: " ++ name) <$> getAnimationMay name

getAnimationMay :: MonadReader Resources m => String -> m (Maybe ThreeDModel)
getAnimationMay name = do
  ms <- view #animations
  pure $ Map.lookup name ms

getSkin :: MonadReader Resources m => String -> m (Map.HashMap String (V.Vector (V.Vector (M44 Float))))
getSkin name = fromMaybe (error $ "Can't find skin: " ++ name) <$> getSkinMay name

getSkinMay :: MonadReader Resources m => String -> m (Maybe (Map.HashMap String (V.Vector (V.Vector (M44 Float)))))
getSkinMay name = do
  ms <- view #skins
  pure $ Map.lookup name ms
