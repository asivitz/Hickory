module Hickory.ImGUI.Helpers where

import DearImGui
import qualified DearImGui.Raw as Raw
import qualified Foreign.C.String as Text
import Control.Exception (bracket)
import GHC.Ptr (nullPtr)
import Linear (V3 (..), V4 (..), V2 (..))
import Control.Lens (iso, Iso')


myWithWindow :: String -> IO (Maybe a) -> IO (Maybe a)
myWithWindow name action = bracket (myBegin name) (const Raw.end) (\b -> if b then action else pure Nothing)

myBegin :: String -> IO Bool
myBegin name =
  Text.withCString name \namePtr ->
    Raw.begin namePtr (Just nullPtr) (Just Raw.ImGuiWindowFlags_MenuBar)

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

v3ImVec3Iso :: Iso' (V3 Float) ImVec3
v3ImVec3Iso = iso v3ToImVec3 imVec3ToV3

v3TripleIso :: Iso' (V3 Float) (Float,Float,Float)
v3TripleIso = iso v3ToTriple tripleToV3

v4ImVec4Iso :: Iso' (V4 Float) ImVec4
v4ImVec4Iso = iso v4ToImVec4 imVec4ToV4
