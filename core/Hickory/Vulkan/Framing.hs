module Hickory.Vulkan.Framing where
import qualified Data.Vector as V
import GHC.Word (Word32)

-- |A mutable resource used to create a frame. Consecutive frames need separate copies of
-- the resource. (For example, if a buffer is modified while rendering
-- a frame, a frame already in flight shouldn't be affected.)

type FramedResource a = V.Vector a

resourceForFrame :: Word32 -> FramedResource a -> a
resourceForFrame i v = v V.! (fromIntegral i `mod` V.length v)

frameResource :: Monad m => m a -> m (FramedResource a)
frameResource = V.replicateM 3 -- TODO: Should match image count in the swapchain

-- This resource is read-only, so it can be duplicated across all frames
unframedResource :: a -> FramedResource a
unframedResource = V.replicate 3

withResourceForFrame :: Word32 -> FramedResource a -> (a -> b) -> b
withResourceForFrame i r f = f (resourceForFrame i r)
