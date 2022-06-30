
module Hickory.Vulkan.Text where
import VulkanMemoryAllocator (Allocation, withMappedMemory, Allocator)
import Vulkan (Buffer, BufferUsageFlagBits (..), MemoryPropertyFlagBits (..))
import Hickory.Vulkan.Vulkan (VulkanResources (..))
import Control.Monad.Managed (Managed)
import Data.Word (Word32)
import Hickory.Vulkan.Mesh (withBuffer', attrStride)
import Foreign ((.|.), sizeOf, copyArray, castPtr)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Vector.Storable as SV
import Control.Exception (bracket)

data DynamicBufferedMesh = DynamicBufferedMesh
  { vertexBufferPair :: (Buffer, Allocation)
  , indexBufferPair  :: (Buffer, Allocation)
  , allocator        :: Allocator -- allocator used to create buffers
  }

withDynamicBufferedMesh :: VulkanResources -> Int -> Managed DynamicBufferedMesh
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

uploadDynamicMesh :: MonadIO m => DynamicBufferedMesh -> SV.Vector Float -> SV.Vector Word32 -> m ()
uploadDynamicMesh DynamicBufferedMesh {..} vertexData indexData = do
  let (_, vertexAlloc) = vertexBufferPair
      (_, indexAlloc)  = indexBufferPair

  liftIO $ withMappedMemory allocator vertexAlloc bracket \bptr ->
    SV.unsafeWith vertexData \vptr -> copyArray (castPtr bptr) vptr (SV.length vertexData)

  liftIO $ withMappedMemory allocator indexAlloc bracket \bptr ->
    SV.unsafeWith indexData \vptr -> copyArray (castPtr bptr) vptr (SV.length indexData)
