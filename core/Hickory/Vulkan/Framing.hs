module Hickory.Vulkan.Framing where

-- |A double buffered resource. Alternating frames need separate copies of
-- the resource. (For example, if a buffer is modified while rendering
-- a frame, a frame already in flight shouldn't be affected.)
data FramedResource a = FramedResource a a

resourceForFrame :: Int -> FramedResource a -> a
resourceForFrame i (FramedResource one _) | even i = one
resourceForFrame _ (FramedResource _ two) = two

frameResource :: Applicative m => m a -> m (FramedResource a)
frameResource a = FramedResource <$> a <*> a