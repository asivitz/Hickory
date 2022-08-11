
module Hickory.Vulkan.Text where
import VulkanMemoryAllocator (Allocation, withMappedMemory, Allocator)
import Vulkan (Buffer, BufferUsageFlagBits (..), MemoryPropertyFlagBits (..))
import Hickory.Vulkan.Vulkan (VulkanResources (..))
import Data.Word (Word32)
import Hickory.Vulkan.Mesh (withBuffer', attrStride, Mesh, indices, pack)
import Foreign ((.|.), sizeOf, copyArray, castPtr)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Vector.Storable as SV
import Control.Exception (bracket)
import Data.Maybe (mapMaybe)
import Acquire.Acquire (Acquire)

data DynamicBufferedMesh = DynamicBufferedMesh
  { vertexBufferPair :: (Buffer, Allocation)
  , indexBufferPair  :: (Buffer, Allocation)
  , allocator        :: Allocator -- allocator used to create buffers
  }

withDynamicBufferedMesh :: VulkanResources -> Int -> Acquire DynamicBufferedMesh
withDynamicBufferedMesh VulkanResources{..} maxVertices = do
  (vertexBuffer, vertexAlloc, _) <- withBuffer' allocator
    BUFFER_USAGE_VERTEX_BUFFER_BIT
    (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
    -- Assume a worst case and make space for every kind of attribute
    (fromIntegral $ sizeOf (undefined :: Float) * (sum . fmap attrStride $ [minBound..maxBound]) * maxVertices)

  (indexBuffer, indexAlloc, _) <- withBuffer' allocator
    BUFFER_USAGE_INDEX_BUFFER_BIT
    (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
    (fromIntegral $ sizeOf (undefined :: Word32) * maxVertices)

  let vertexBufferPair = (vertexBuffer, vertexAlloc)
      indexBufferPair  = (indexBuffer, indexAlloc)
  pure DynamicBufferedMesh {..}

uploadDynamicMesh :: MonadIO m => DynamicBufferedMesh -> [Mesh] -> m ()
uploadDynamicMesh DynamicBufferedMesh {..} meshes = do
  let (_, vertexAlloc) = vertexBufferPair
      (_, indexAlloc)  = indexBufferPair
      indexData  = SV.concat $ mapMaybe indices meshes
      vertexData = SV.concat $ map pack meshes

  liftIO $ withMappedMemory allocator vertexAlloc bracket \bptr ->
    SV.unsafeWith vertexData \vptr -> copyArray (castPtr bptr) vptr (SV.length vertexData)

  liftIO $ withMappedMemory allocator indexAlloc bracket \bptr ->
    SV.unsafeWith indexData \vptr -> copyArray (castPtr bptr) vptr (SV.length indexData)
