{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Hickory.Editor.GUI where

import qualified Data.HashMap.Strict as Map
import DearImGui (withMenuBarOpen, withMenuOpen, menuItem, collapsingHeader, dragFloat3, colorEdit4, treePop, withDragDropTarget, withDragDropSource, isItemClicked, pattern ImGuiTreeNodeFlags_Selected, treeNodeWith, inputText, checkbox, dragFloat, withComboOpen, selectable, button, dragInt, dragFloat2, pattern ImGuiTreeNodeFlags_Leaf, ImGuiMouseButton (..))
import Hickory.ImGUI.Helpers (myWithWindow, v3ToTriple, tripleToV3, v2ToTuple, tupleToV2, imVec4ToV4, v4ToImVec4)
import Control.Monad.Extra (whenM)
import Data.Bits (zeroBits, (.|.))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Hickory.Editor.Types
import Data.IORef (modifyIORef', readIORef, IORef)
import Data.Functor (void)
import Control.Monad (when)
import Data.Text (pack, unpack)
import Data.Foldable (for_, foldl')
import Data.Functor.Const (Const(..))
import Data.Maybe (fromMaybe, mapMaybe, isNothing)
import Type.Reflection (type (:~~:) (..))
import Control.Lens (ifor_, preview, to, Traversal', over, ix, _2, Identity (..), at)
import Safe (headMay)
import Data.StateVar (makeStateVar, StateVar)
import Linear (translation)
import Hickory.Editor.General (setScale, matScale, matEuler)
import Text.Printf (printf)
import Data.Word (Word32)
