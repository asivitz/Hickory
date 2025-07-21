{-# LANGUAGE BlockArguments #-}

module Hickory.ImGUI where

import DearImGui
import qualified DearImGui.Raw as Raw
import qualified Foreign.C.String as Text
import Control.Exception (bracket)
import GHC.Ptr (nullPtr)
import Linear (V3 (..), V4 (..), V2 (..))


myWithWindow :: String -> IO (Maybe a) -> IO (Maybe a)
myWithWindow name action = bracket (myBegin name) (const Raw.end) (\b -> if b then action else pure Nothing)

myBegin :: String -> IO Bool
myBegin name =
  Text.withCString name \namePtr ->
    Raw.begin namePtr (Just nullPtr) (Just Raw.ImGuiWindowFlags_MenuBar)

imVec4ToV4 :: ImVec4 -> V4 Float
imVec4ToV4 (ImVec4 x y z w) = V4 x y z w

v4ToImVec4 :: V4 Float -> ImVec4
v4ToImVec4 (V4 r g b a) = ImVec4 r g b a

instance Eq ImVec4 where
  a == b = imVec4ToV4 a == imVec4ToV4 b
