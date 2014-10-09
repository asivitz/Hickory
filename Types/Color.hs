module Types.Color where

import Math.Vector

type Color = Vector4

white :: Color
white = Vector4 1 1 1 1

black :: Color
black = Vector4 0 0 0 1

rgb :: Scalar -> Scalar -> Scalar -> Color
rgb r g b = rgba r g b 1

rgba :: Scalar -> Scalar -> Scalar -> Scalar -> Color
rgba r g b a = case vpack $ fmap realToFrac [r,g,b,a] of
                   Just v -> v
                   Nothing -> error "Should never happen"

rgb255 :: Int -> Int -> Int -> Color
rgb255 r g b = rgb ((realToFrac r)/255) ((realToFrac g)/255) ((realToFrac b)/255)

changeAlpha :: Scalar -> Color -> Color
changeAlpha a (Vector4 r g b _) = v4 r g b a
