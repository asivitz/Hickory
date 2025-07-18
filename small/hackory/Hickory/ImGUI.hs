module Hickory.ImGUI where

import DearImGui
import qualified DearImGui.Raw as Raw
import qualified Foreign.C.String as Text
import Control.Exception (bracket)
import GHC.Ptr (nullPtr)
import Linear (V3 (..), V4 (..), V2 (..))


imVec3ToColor :: ImVec3 -> V4 Float
imVec3ToColor (ImVec3 x y z) = V4 x y z 1

imVec3ToV3 :: ImVec3 -> V3 Float
imVec3ToV3 (ImVec3 x y z) = V3 x y z

imVec4ToV4 :: ImVec4 -> V4 Float
imVec4ToV4 (ImVec4 x y z w) = V4 x y z w

tripleToV3 :: (Float, Float, Float) -> V3 Float
tripleToV3 (x,y,z) = V3 x y z

quadToV4 :: (Float, Float, Float, Float) -> V4 Float
quadToV4 (x,y,z,w) = V4 x y z w

tupleToV2 :: (Float, Float) -> V2 Float
tupleToV2 (x,y) = V2 x y

colorToImVec3 :: V4 Float -> ImVec3
colorToImVec3 (V4 r g b _a) = ImVec3 r g b

v3ToImVec3 :: V3 Float -> ImVec3
v3ToImVec3 (V3 r g b) = ImVec3 r g b

v4ToImVec4 :: V4 Float -> ImVec4
v4ToImVec4 (V4 r g b a) = ImVec4 r g b a

v4ToQuad :: V4 Float -> (Float, Float, Float, Float)
v4ToQuad (V4 r g b a) = (r,g,b,a)

v3ToTriple :: V3 Float -> (Float, Float, Float)
v3ToTriple (V3 r g b) = (r,g,b)

v2ToTuple :: V2 Float -> (Float, Float)
v2ToTuple (V2 r g) = (r,g)

instance Eq ImVec4 where
  a == b = imVec4ToV4 a == imVec4ToV4 b
