module Types.Color where

import Math.Matrix

type Color = Vec4

white :: Color
white = fromList [1, 1, 1, 1]

black :: Color
black = fromList [0, 0, 0, 1]

rgb :: Float -> Float -> Float -> Color
rgb r g b = rgba r g b 1

rgba :: Float -> Float -> Float -> Float -> Color
rgba r g b a = fromList $ fmap realToFrac [r,g,b,a]
