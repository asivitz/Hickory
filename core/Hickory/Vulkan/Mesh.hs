{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE DataKinds, PatternSynonyms  #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Hickory.Vulkan.Mesh where

import Data.Binary
import Data.Vector.Binary ()
import Data.Vector.Storable as SV
import Data.Vector as V
import Data.Functor ((<&>))
import Vulkan (VertexInputBindingDescription (..), VertexInputRate (..), VertexInputAttributeDescription (..), Format (..), BufferCreateInfo(..), MemoryPropertyFlags, DeviceSize, Buffer, SharingMode (..), BufferUsageFlags, MemoryPropertyFlagBits (..), BufferUsageFlagBits (..), CommandBufferAllocateInfo(..), CommandBufferLevel (..), withCommandBuffers, SubmitInfo(..), BufferCopy(..), useCommandBuffer, cmdCopyBuffer, queueSubmit, commandBufferHandle, pattern COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT, CommandBufferBeginInfo(..), queueWaitIdle, CommandBuffer
  )
import Foreign (sizeOf, (.|.), castPtr)
import Hickory.Vulkan.Vulkan (mkAcquire, runAcquire)
import Vulkan.Zero (zero)
import VulkanMemoryAllocator (AllocationCreateInfo(requiredFlags), Allocator, Allocation, AllocationInfo, withMappedMemory)
import qualified VulkanMemoryAllocator as VMA
import Control.Exception (bracket)
import Foreign.Marshal.Array (copyArray)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Vulkan.CStruct.Extends (SomeStruct(..))
import Data.List (sortOn)
import Acquire.Acquire (Acquire)
import Hickory.Vulkan.Types (Mesh (..), Attribute (..), VulkanResources (..), DeviceContext (..), BufferedMesh (..))

writeMeshToFile :: FilePath -> Mesh -> IO ()
writeMeshToFile = encodeFile

loadMeshFromFile :: FilePath -> IO Mesh
loadMeshFromFile = decodeFile

instance Binary Attribute
instance Binary Mesh

attrStride :: Attribute -> Int
attrStride Position      = 3
attrStride Normal        = 3
attrStride TextureCoord  = 2
attrStride Color         = 4
attrStride BoneIndex     = 1
attrStride MaterialIndex = 1
attrStride JointIndices  = 4
attrStride JointWeights  = 4

attrLocation :: Attribute -> Word32
attrLocation Position      = 0
attrLocation Color         = 1
attrLocation Normal        = 2
attrLocation TextureCoord  = 3
attrLocation BoneIndex     = 4
attrLocation MaterialIndex = 5
attrLocation JointIndices  = 6
attrLocation JointWeights  = 7

attrFormat :: Attribute -> Format
attrFormat Position      = FORMAT_R32G32B32_SFLOAT
attrFormat Normal        = FORMAT_R32G32B32_SFLOAT
attrFormat TextureCoord  = FORMAT_R32G32_SFLOAT
attrFormat Color         = FORMAT_R32G32B32A32_SFLOAT
attrFormat BoneIndex     = FORMAT_R32_SFLOAT
attrFormat MaterialIndex = FORMAT_R32_SFLOAT
attrFormat JointIndices  = FORMAT_R32G32B32A32_SFLOAT
attrFormat JointWeights  = FORMAT_R32G32B32A32_SFLOAT

pack :: Mesh -> SV.Vector Float
pack Mesh {..} = SV.concat $ fmap snd . sortOn (attrLocation . fst) $ vertices

numVerts :: Mesh -> Int
numVerts Mesh { vertices = ((attr, vec):_) } =
  let (num, remainder) = SV.length vec `quotRem` attrStride attr
  in if remainder == 0 then num else error "Invalid mesh. Attribute not evenly divisible by stride."
numVerts _ = 0

meshAttributes :: Mesh -> [Attribute]
meshAttributes = fmap fst . vertices

bindingDescriptions :: [Attribute] -> V.Vector VertexInputBindingDescription
bindingDescriptions attrs = V.fromList $ Prelude.zip [0..] attrs <&> \(i,a) -> VertexInputBindingDescription
  { binding = i
  , stride = fromIntegral $ attrStride a * sizeOf (0 :: Float)
  , inputRate = VERTEX_INPUT_RATE_VERTEX
  }

attributeDescriptions :: [Attribute] -> V.Vector VertexInputAttributeDescription
attributeDescriptions attrs = V.fromList $ Prelude.zip [0..] attrs <&> \(i,a) -> VertexInputAttributeDescription
  { binding = i
  , location = attrLocation a
  , format = attrFormat a
  , offset = 0
  }

withBufferedMesh :: VulkanResources -> Mesh -> Acquire BufferedMesh
withBufferedMesh bag mesh@Mesh {..} = do
  vertexBuffer <- withVertexBuffer bag (pack mesh)
  indexBuffer  <- traverse (withIndexBuffer bag) indices
  pure BufferedMesh {..}

{- Buffer Utils -}

withBuffer' :: Allocator -> BufferUsageFlags -> MemoryPropertyFlags -> DeviceSize -> Acquire (Buffer,Allocation,AllocationInfo)
withBuffer' allocator usageFlags requiredFlags size = VMA.withBuffer allocator bufferCreateInfo allocInfo mkAcquire
  where
  bufferCreateInfo = zero
    { size        = size
    , usage       = usageFlags
    , sharingMode = SHARING_MODE_EXCLUSIVE
    }
  allocInfo = zero { requiredFlags = requiredFlags }


withVertexBuffer :: Storable a => VulkanResources -> SV.Vector a -> Acquire Buffer
withVertexBuffer bag = withBuffer bag BUFFER_USAGE_VERTEX_BUFFER_BIT

withIndexBuffer :: Storable a => VulkanResources -> SV.Vector a -> Acquire Buffer
withIndexBuffer bag = withBuffer bag BUFFER_USAGE_INDEX_BUFFER_BIT

vsizeOf :: Storable a => SV.Vector a -> Word32
vsizeOf v = fromIntegral $ SV.length v * sizeOf (SV.head v)

withBuffer :: Storable a => VulkanResources -> BufferUsageFlags -> SV.Vector a -> Acquire Buffer
withBuffer vr@VulkanResources {..} usageFlags dat = do
  let bufferSize = fromIntegral $ vsizeOf dat
  -- Rather than copying directly from CPU to GPU, we want the buffer to
  -- live in memory only accesible from GPU for better peformance.
  -- So we set up a staging buffer, transfer from host to staging,
  -- and then go from staging to optimized memory.

  -- Set up the staging buffer
  (stagingBuffer, stagingAlloc, _) <- withBuffer' allocator
    BUFFER_USAGE_TRANSFER_SRC_BIT
    (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
    bufferSize

  liftIO $ withMappedMemory allocator stagingAlloc bracket \bptr -> do
    SV.unsafeWith dat \vptr ->
      copyArray (castPtr bptr) vptr (SV.length dat)

  -- Set up the real buffer on the GPU and copy from the staging buffer
  (buffer, _, _) <- withBuffer' allocator
    (BUFFER_USAGE_TRANSFER_DST_BIT .|. usageFlags)
    MEMORY_PROPERTY_DEVICE_LOCAL_BIT
    bufferSize

  copyBuffer vr stagingBuffer buffer bufferSize

  pure buffer

copyBuffer :: MonadIO m => VulkanResources -> Buffer -> Buffer -> DeviceSize -> m ()
copyBuffer bag srcBuf dstBuf bufferSize = withSingleTimeCommands bag \commandBuffer -> do
    let copyInfo :: BufferCopy
        copyInfo = zero { size = bufferSize }
    cmdCopyBuffer commandBuffer srcBuf dstBuf [copyInfo]

withSingleTimeCommands :: MonadIO m => VulkanResources -> (CommandBuffer -> IO ()) -> m ()
withSingleTimeCommands VulkanResources {..} f = liftIO $ runAcquire do
  let DeviceContext {..} = deviceContext

  -- Need a temporary command buffer for copy commands
  commandBuffer <- V.head <$>
    let commandBufferAllocateInfo :: CommandBufferAllocateInfo
        commandBufferAllocateInfo = zero
          { commandPool        = shortLivedCommandPool
          , level              = COMMAND_BUFFER_LEVEL_PRIMARY
          , commandBufferCount = 1
          }
    in withCommandBuffers device commandBufferAllocateInfo mkAcquire

  let beginInfo :: CommandBufferBeginInfo '[]
      beginInfo = zero { flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT }
  useCommandBuffer commandBuffer beginInfo do
    liftIO $ f commandBuffer

  let submitInfo = zero { commandBuffers = [commandBufferHandle commandBuffer] }
  queueSubmit graphicsQueue [SomeStruct submitInfo] zero
  queueWaitIdle graphicsQueue
