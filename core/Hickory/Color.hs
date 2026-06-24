module Hickory.Color where

import Linear (V4(..))

type Color = V4 Float

white :: Color
white = V4 1 1 1 1

black :: Color
black = V4 0 0 0 1

red :: Color
red = V4 1 0 0 1

green :: Color
green = V4 0 1 0 1

blue :: Color
blue = V4 0 0 1 1

rgb :: Float -> Float -> Float -> Color
rgb r g b = rgba r g b 1

rgba :: Float -> Float -> Float -> Float -> Color
rgba = V4

rgb255 :: Int -> Int -> Int -> Color
rgb255 r g b = rgb (realToFrac r / 255) (realToFrac g / 255) (realToFrac b / 255)

changeAlpha :: Float -> Color -> Color
changeAlpha a (V4 r g b _) = V4 r g b a
