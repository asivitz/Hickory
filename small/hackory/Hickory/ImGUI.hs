module Hickory.ImGUI where

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
