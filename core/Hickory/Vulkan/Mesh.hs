{-# LANGUAGE DeriveGeneric #-}

module Hickory.Vulkan.Mesh where

import Data.Binary
import Data.Vector.Binary ()
import Data.Vector.Storable as SV
import Data.Vector as V
import Data.Functor ((<&>))
import Vulkan (VertexInputBindingDescription (..), VertexInputRate (..), VertexInputAttributeDescription (..), Format (..), BufferCreateInfo(..), MemoryPropertyFlags, DeviceSize, Buffer, SharingMode (..), BufferUsageFlags, MemoryPropertyFlagBits (..), BufferUsageFlagBits (..))
import Foreign (sizeOf, (.|.), castPtr)
import qualified Data.List as List
import GHC.Generics (Generic)
import Hickory.Vulkan.Vulkan (allocate)
import Vulkan.Zero (zero)
import VulkanMemoryAllocator (AllocationCreateInfo(requiredFlags), Allocator, Allocation, AllocationInfo, withMappedMemory)
import qualified VulkanMemoryAllocator as VMA
import Control.Monad.Managed (Managed, liftIO)
import Control.Exception (bracket)
import Foreign.Marshal.Array (copyArray)

type Mesh = [(Attribute, SV.Vector Float)]

writeToFile :: FilePath -> Mesh -> IO ()
writeToFile = encodeFile

data Attribute
  = Position
  | Normal
  | TextureCoord
  | Color
  deriving Generic

instance Binary Attribute

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
pack mesh = SV.concat $ packVert <$> [0..(numVerts mesh - 1)]
  where
  packVert i = SV.concat $ mesh <&> \(attr, v) -> let str = attrStride attr in SV.fromList $ [0..str - 1] <&> \idx -> v SV.! (i * str + idx)

numVerts :: Mesh -> Int
numVerts ((attr, vec):_) =
  let (num, remainder) = SV.length vec `quotRem` attrStride attr
  in if remainder == 0 then num else error "Invalid mesh. Attribute not evenly divisible by stride."
numVerts _ = 0

meshBindingDescription :: Mesh -> VertexInputBindingDescription
meshBindingDescription mesh = VertexInputBindingDescription
  { binding = 0
  , stride = fromIntegral $ Prelude.sum (attrStride . fst <$> mesh) * sizeOf (0 :: Float)
  , inputRate = VERTEX_INPUT_RATE_VERTEX
  }

meshAttributeDescriptions :: Mesh -> V.Vector VertexInputAttributeDescription
meshAttributeDescriptions = V.fromList . snd. List.mapAccumL mk 0
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

withBuffer :: Allocator -> BufferUsageFlags -> MemoryPropertyFlags -> DeviceSize -> Managed (Buffer,Allocation,AllocationInfo)
withBuffer allocator usageFlags requiredFlags size = VMA.withBuffer allocator bufferCreateInfo allocInfo allocate
  where
  bufferCreateInfo = zero
    { size               = size
    , usage              = usageFlags
    , sharingMode        = SHARING_MODE_EXCLUSIVE
    }
  allocInfo = zero { requiredFlags = requiredFlags }


withMeshBuffer :: Allocator -> Mesh -> Managed Buffer
withMeshBuffer allocator mesh = do
  let packed = pack mesh
      bufferSize = fromIntegral $ SV.length packed * sizeOf (SV.head packed)
      memFlags = MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT
  (buffer, alloc, _) <- withBuffer allocator BUFFER_USAGE_VERTEX_BUFFER_BIT memFlags bufferSize

  liftIO $ withMappedMemory allocator alloc bracket \bptr -> do
    SV.unsafeWith packed \vptr ->
      copyArray (castPtr bptr) vptr (SV.length packed)

  pure buffer
