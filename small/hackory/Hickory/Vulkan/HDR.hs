{-# LANGUAGE TypeFamilies #-}
module Hickory.Vulkan.HDR where

import Data.Dynamic (Typeable)
import Codec.Picture (PixelF, Pixel(..), Image (..))
import Codec.Picture.Types (MutableImage(..))
import Data.Vector.Storable ((!))
import qualified Data.Vector.Storable.Mutable as M
import qualified Data.Vector.Storable as V
import Control.Monad (liftM, ap)

-- The relevant vulkan format for HDR is R32_G32_B32_A32
-- But the JuicyPixels library loads HDR into RGBF (3 components rather than 4)
-- So we have to make a custom pixel format and translate
--
data PixelRGBAF = PixelRGBAF {-# UNPACK #-} !PixelF -- Red
                             {-# UNPACK #-} !PixelF -- Green
                             {-# UNPACK #-} !PixelF -- Blue
                             {-# UNPACK #-} !PixelF -- Alpha
  deriving (Eq, Ord, Show, Typeable)

instance Pixel PixelRGBAF where
  type PixelBaseComponent PixelRGBAF = PixelF

  {-# INLINE pixelOpacity #-}
  pixelOpacity = const 1.0

  {-# INLINE mixWith #-}
  mixWith f (PixelRGBAF ra ga ba aa) (PixelRGBAF rb gb bb ab) =
      PixelRGBAF (f 0 ra rb) (f 1 ga gb) (f 2 ba bb) (f 3 aa ab)

  {-# INLINE colorMap #-}
  colorMap f (PixelRGBAF r g b a) = PixelRGBAF (f r) (f g) (f b) (f a)

  {-# INLINE componentCount #-}
  componentCount _ = 4

  {-# INLINE pixelAt #-}
  pixelAt image@(Image { imageData = arr }) x y = PixelRGBAF (arr ! (baseIdx + 0))
                                                             (arr ! (baseIdx + 1))
                                                             (arr ! (baseIdx + 2))
                                                             (arr ! (baseIdx + 3))
      where baseIdx = pixelBaseIndex image x y

  {-# INLINE readPixel #-}
  readPixel image@(MutableImage { mutableImageData = arr }) x y = do
      rv <- arr `M.read` baseIdx
      gv <- arr `M.read` (baseIdx + 1)
      bv <- arr `M.read` (baseIdx + 2)
      av <- arr `M.read` (baseIdx + 3)
      return $ PixelRGBAF rv gv bv av
      where baseIdx = mutablePixelBaseIndex image x y

  {-# INLINE writePixel #-}
  writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelRGBAF rv gv bv av) = do
      let baseIdx = mutablePixelBaseIndex image x y
      (arr `M.write` (baseIdx + 0)) rv
      (arr `M.write` (baseIdx + 1)) gv
      (arr `M.write` (baseIdx + 2)) bv
      (arr `M.write` (baseIdx + 3)) av

  {-# INLINE unsafePixelAt #-}
  unsafePixelAt v idx =
      PixelRGBAF (V.unsafeIndex v idx) (V.unsafeIndex v $ idx + 1) (V.unsafeIndex v $ idx + 2) (V.unsafeIndex v $ idx + 3)
  {-# INLINE unsafeReadPixel #-}
  unsafeReadPixel vec idx =
      PixelRGBAF `liftM` M.unsafeRead vec idx
                 `ap` M.unsafeRead vec (idx + 1)
                 `ap` M.unsafeRead vec (idx + 2)
                 `ap` M.unsafeRead vec (idx + 3)
  {-# INLINE unsafeWritePixel #-}
  unsafeWritePixel v idx (PixelRGBAF r g b a) =
      M.unsafeWrite v idx r >> M.unsafeWrite v (idx + 1) g
                            >> M.unsafeWrite v (idx + 2) b
                            >> M.unsafeWrite v (idx + 3) a
