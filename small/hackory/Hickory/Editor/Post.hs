{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels, OverloadedRecordDot #-}

module Hickory.Editor.Post where

import DearImGui
    ( withMenuBarOpen,
      withMenuOpen,
      menuItem,
      dragFloat3,
      dragFloat,
      colorEdit3, ImVec2 (..), ImVec4 (..), image, collapsingHeader, dragInt, checkbox )
import Data.IORef ( IORef, readIORef, modifyIORef' )
import GHC.Generics (Generic)
import Hickory.ImGUI.Helpers (myWithWindow, v3TripleIso)
import Control.Monad.Extra (whenM)
import Linear (V3 (..))
import Control.Monad (void)
import Vulkan (Extent2D (..), objectTypeAndHandle)
import Foreign (with, castPtr, wordPtrToPtr, WordPtr(..))
import Control.Lens (view, set, Lens')
import Hickory.Vulkan.Types (FrameContext(..), RenderConfig(..))
import Hickory.Vulkan.Framing (resourceForFrame)
import Data.Bits
import Data.StateVar (makeStateVar, StateVar)
import Hickory.ImGUI.Helpers (v3ImVec3Iso)
