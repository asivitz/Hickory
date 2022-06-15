{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE DataKinds, PatternSynonyms  #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Hickory.Vulkan.Mesh where

import Control.Monad.Managed ( runManaged, Managed, liftIO )
import Data.Binary
import Data.Vector.Binary ()
import Data.Vector.Storable as SV
import Data.Vector as V
import Data.Functor ((<&>))
import Vulkan (VertexInputBindingDescription (..), VertexInputRate (..), VertexInputAttributeDescription (..), Format (..), BufferCreateInfo(..), MemoryPropertyFlags, DeviceSize, Buffer, SharingMode (..), BufferUsageFlags, MemoryPropertyFlagBits (..), BufferUsageFlagBits (..), CommandBufferAllocateInfo(..), CommandBufferLevel (..), withCommandBuffers, SubmitInfo(..), BufferCopy(..), useCommandBuffer, cmdCopyBuffer, queueSubmit, commandBufferHandle, pattern COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT, CommandBufferBeginInfo(..), queueWaitIdle, CommandBuffer, cmdBindVertexBuffers, cmdBindIndexBuffer, cmdDrawIndexed, cmdDraw, pattern INDEX_TYPE_UINT32)
import Foreign (sizeOf, (.|.), castPtr)
import qualified Data.List as List
import GHC.Generics (Generic)
import Hickory.Vulkan.Vulkan (allocate, Bag (..), DeviceContext (..))
import Vulkan.Zero (zero)
import VulkanMemoryAllocator (AllocationCreateInfo(requiredFlags), Allocator, Allocation, AllocationInfo, withMappedMemory)
import qualified VulkanMemoryAllocator as VMA
import Control.Exception (bracket)
import Foreign.Marshal.Array (copyArray)
import Control.Monad.IO.Class (MonadIO)
import Vulkan.CStruct.Extends (SomeStruct(..))

data Mesh = Mesh
  { vertices :: [(Attribute, SV.Vector Float)]
  , indices :: Maybe (SV.Vector Word32)
  } deriving Generic

data BufferedMesh = BufferedMesh
  { mesh         :: Mesh
  , vertexBuffer :: Buffer
  , indexBuffer  :: Maybe Buffer
  }

writeToFile :: FilePath -> Mesh -> IO ()
writeToFile = encodeFile

data Attribute
  = Position
  | Normal
  | TextureCoord
  | Color
  deriving Generic

instance Binary Attribute
instance Binary Mesh

attrStride :: Attribute -> Int
attrStride Position     = 3
attrStride Normal       = 3
attrStride TextureCoord = 2
attrStride Color        = 3

attrLocation :: Attribute -> Word32
attrLocation Position     = 0
attrLocation Color        = 1
attrLocation Normal       = 2
attrLocation TextureCoord = 3

attrFormat :: Attribute -> Format
attrFormat Position     = FORMAT_R32G32B32_SFLOAT
attrFormat Normal       = FORMAT_R32G32B32_SFLOAT
attrFormat TextureCoord = FORMAT_R32G32_SFLOAT
attrFormat Color        = FORMAT_R32G32B32_SFLOAT

pack :: Mesh -> SV.Vector Float
pack mesh@Mesh {..} = SV.concat $ packVert <$> [0..(numVerts mesh - 1)]
  where
  packVert i = SV.concat $ vertices <&> \(attr, v) -> let str = attrStride attr in SV.fromList $ [0..str - 1] <&> \idx -> v SV.! (i * str + idx)

numVerts :: Mesh -> Int
numVerts Mesh { vertices = ((attr, vec):_) } =
  let (num, remainder) = SV.length vec `quotRem` attrStride attr
  in if remainder == 0 then num else error "Invalid mesh. Attribute not evenly divisible by stride."
numVerts _ = 0

meshBindingDescription :: Mesh -> VertexInputBindingDescription
meshBindingDescription Mesh { vertices } = VertexInputBindingDescription
  { binding = 0
  , stride = fromIntegral $ Prelude.sum (attrStride . fst <$> vertices) * sizeOf (0 :: Float)
  , inputRate = VERTEX_INPUT_RATE_VERTEX
  }

meshAttributeDescriptions :: Mesh -> V.Vector VertexInputAttributeDescription
meshAttributeDescriptions = V.fromList . snd . List.mapAccumL mk 0 . vertices
  where
  mk stride' (attr, _) =
    (stride' + attrStride attr
    , VertexInputAttributeDescription
      { binding = 0
      , location = attrLocation attr
      , format = attrFormat attr
      , offset = fromIntegral $ stride' * sizeOf (0 :: Float)
      }
    )

withBufferedMesh :: Bag -> Mesh -> Managed BufferedMesh
withBufferedMesh bag mesh@Mesh {..} = do
  vertexBuffer <- withVertexBuffer bag (pack mesh)
  indexBuffer  <- traverse (withIndexBuffer bag) indices
  pure BufferedMesh {..}

cmdDrawBufferedMesh :: MonadIO m => CommandBuffer -> BufferedMesh -> m ()
cmdDrawBufferedMesh commandBuffer BufferedMesh {..} = do
  cmdBindVertexBuffers commandBuffer 0 [vertexBuffer] [0]
  case (indices mesh, indexBuffer) of
    (Just is, Just ibuf) -> do
      cmdBindIndexBuffer commandBuffer ibuf 0 INDEX_TYPE_UINT32
      cmdDrawIndexed commandBuffer (fromIntegral . SV.length $ is) 1 0 0 0
    (Nothing, Nothing) -> do
      cmdDraw commandBuffer (fromIntegral $ numVerts mesh) 1 0 0
    _ -> error "Mesh has indices but they aren't buffered."

{- Buffer Utils -}

withBuffer' :: Allocator -> BufferUsageFlags -> MemoryPropertyFlags -> DeviceSize -> Managed (Buffer,Allocation,AllocationInfo)
withBuffer' allocator usageFlags requiredFlags size = VMA.withBuffer allocator bufferCreateInfo allocInfo allocate
  where
  bufferCreateInfo = zero
    { size        = size
    , usage       = usageFlags
    , sharingMode = SHARING_MODE_EXCLUSIVE
    }
  allocInfo = zero { requiredFlags = requiredFlags }


withVertexBuffer :: Storable a => Bag -> SV.Vector a -> Managed Buffer
withVertexBuffer bag = withBuffer bag BUFFER_USAGE_VERTEX_BUFFER_BIT

withIndexBuffer :: Storable a => Bag -> SV.Vector a -> Managed Buffer
withIndexBuffer bag = withBuffer bag BUFFER_USAGE_INDEX_BUFFER_BIT

vsizeOf :: Storable a => SV.Vector a -> Word32
vsizeOf v = fromIntegral $ SV.length v * sizeOf (SV.head v)

withBuffer :: Storable a => Bag -> BufferUsageFlags -> SV.Vector a -> Managed Buffer
withBuffer bag@Bag {..} usageFlags dat = do
  let bufferSize = fromIntegral $ vsizeOf dat
  -- Rather than copying directly from CPU to GPU, we want the buffer to
  -- live in memory only accesible from GPU for better peformance.
  -- So we set up a staging buffer on the GPU, transfer from CPU to staging,
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

  copyBuffer bag stagingBuffer buffer bufferSize

  pure buffer

copyBuffer :: MonadIO m => Bag -> Buffer -> Buffer -> DeviceSize -> m ()
copyBuffer Bag {..} srcBuf dstBuf bufferSize = liftIO $ runManaged do
  let DeviceContext {..} = deviceContext

  -- Need a temporary command buffer for copy commands
  commandBuffer <- V.head <$>
    let commandBufferAllocateInfo :: CommandBufferAllocateInfo
        commandBufferAllocateInfo = zero
          { commandPool        = shortLivedCommandPool
          , level              = COMMAND_BUFFER_LEVEL_PRIMARY
          , commandBufferCount = 1
          }
    in withCommandBuffers device commandBufferAllocateInfo allocate

  let beginInfo :: CommandBufferBeginInfo '[]
      beginInfo = zero { flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT }
  useCommandBuffer commandBuffer beginInfo do
    let copyInfo :: BufferCopy
        copyInfo = zero { size = bufferSize }
    cmdCopyBuffer commandBuffer srcBuf dstBuf [copyInfo]

  let submitInfo = zero { commandBuffers = [commandBufferHandle commandBuffer] }
  queueSubmit graphicsQueue [SomeStruct submitInfo] zero
  queueWaitIdle graphicsQueue
