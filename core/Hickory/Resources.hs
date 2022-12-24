{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}

module Hickory.Resources where

import qualified Data.HashMap.Strict as Map
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, readIORef, newIORef, modifyIORef')
import Acquire.Acquire (Acquire)
import Data.Hashable (Hashable (..))
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
import System.FilePath.Lens (extension)
import Control.Monad.Reader.Class (MonadReader)
import Data.Maybe (fromMaybe, listToMaybe)
import System.Directory (doesFileExist)
import Hickory.Vulkan.StockMesh (withCubeMesh, withSquareMesh)
import Hickory.Vulkan.StockTexture (withWhiteImageDescriptor)

type ResourceStore a b options = (a -> options -> Acquire (Maybe b), IORef (Map.HashMap a (b, IO ())))

newResourceStore :: (Eq a, Hashable a) => (a -> options -> Acquire (Maybe b)) -> IO (ResourceStore a b options)
newResourceStore f = (f,) <$> newIORef mempty

cleanupStore :: ResourceStore a b options -> IO ()
cleanupStore (_,ref) = do
  m <- readIORef ref
  sequence_ $ snd . snd <$> Map.toList m

loadResource :: (Show a, Eq a, Hashable a) => ResourceStore a b options -> a -> options -> IO ()
loadResource (f,ref) k options = do
  m <- readIORef ref
  unless (Map.member k m) do
    unWrapAcquire (f k options) >>= \case
      (Just res, cleanup) -> liftIO $ modifyIORef' ref $ Map.insert k (res, cleanup)
      (Nothing, cleanup) -> cleanup

loadResource' :: (Eq a, Hashable a) => ResourceStore a b options -> a -> Acquire b -> IO ()
loadResource' (_,ref) k f = do
  res <- unWrapAcquire f
  liftIO $ modifyIORef' ref $ Map.insert k res

getResources :: ResourceStore a b options -> IO (Map.HashMap a b)
getResources = fmap (Map.map fst) . readIORef . snd

data ResourcesStore = ResourcesStore
  { meshes     :: ResourceStore String BufferedMesh ()
  , textures   :: ResourceStore String PointedDescriptorSet (Filter, SamplerAddressMode)
  , fonts      :: ResourceStore String TextRenderer Float
  , animations :: ResourceStore String ThreeDModel ()
  } deriving (Generic)

data Resources = Resources
  { meshes     :: Map.HashMap String BufferedMesh
  , textures   :: Map.HashMap String PointedDescriptorSet
  , fonts      :: Map.HashMap String TextRenderer
  , animations :: Map.HashMap String ThreeDModel
  } deriving (Generic)

instance Hashable SamplerAddressMode where hashWithSalt s (SamplerAddressMode i) = hashWithSalt s i
instance Hashable Filter where hashWithSalt s (Filter i) = hashWithSalt s i

withResourcesStore :: VulkanResources -> FilePath -> Acquire ResourcesStore
withResourcesStore vulkanResources path = do
  textures <- liftIO $ newResourceStore \name (imageFilter, addressMode) -> do
    let fp = path ++ "/images/" ++ name
    liftIO (doesFileExist fp) >>= \case
      True -> fmap Just $ view #descriptorSet <$> withTextureDescriptorSet vulkanResources [(path ++ "images/" ++ name, imageFilter, addressMode)]
      False -> pure Nothing

  meshes <- liftIO $ newResourceStore \name _ -> do
    let fp = path ++ "/models/" ++ name
    liftIO (doesFileExist fp) >>= \case
      True ->
        case name ^. extension of
        ".hmdl" -> fmap Just $ liftIO (loadMeshFromFile fp) >>= withBufferedMesh vulkanResources
        ".x"    -> fmap Just $ liftIO (loadModelFromX   fp) >>= withBufferedMesh vulkanResources . packedMesh
        _       -> pure Nothing
      False -> pure Nothing

  liftIO do
    loadResource' textures "white" do
      des <- withWhiteImageDescriptor vulkanResources
      withDescriptorSet vulkanResources [des]
    loadResource' meshes "square" (withSquareMesh vulkanResources)
    loadResource' meshes "cube" (withCubeMesh vulkanResources)

  fonts <- liftIO $ newResourceStore \name msdfDist -> do
    let fontfp = path ++ "/fonts/" ++ name ++ ".json"
        imagefp = path ++ "/images/" ++ name ++ ".png"
    liftIO ((&&) <$> doesFileExist fontfp <*> doesFileExist imagefp) >>= \case
      True -> Just <$> withTextRenderer vulkanResources fontfp imagefp msdfDist
      False -> pure Nothing

  animations <- liftIO $ newResourceStore \name _ -> do
    let fp = path ++ "/models/" ++ name
    liftIO $ doesFileExist fp >>= \case
      True -> Just <$> loadModelFromX fp
      False -> pure Nothing

  mkAcquire (pure ()) (const $ cleanupStore meshes)
  mkAcquire (pure ()) (const $ cleanupStore textures)
  mkAcquire (pure ()) (const $ cleanupStore fonts)
  mkAcquire (pure ()) (const $ cleanupStore animations)

  pure ResourcesStore {..}

getResourcesStoreResources :: ResourcesStore -> IO Resources
getResourcesStoreResources ResourcesStore {..} =
  Resources <$> getResources meshes
            <*> getResources textures
            <*> getResources fonts
            <*> getResources animations

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
