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
import Hickory.Editor.General (setScale, setRotation, matScale, matEuler)
import Text.Printf (printf)
import Hickory.Utils.Utils (deleteAt)
import Data.Word (Word32)

drawMainEditorUI :: FilePath -> HashMap Word32 Object -> HashMap Word32 Object -> (Word32 -> IO ()) -> IO ()
drawMainEditorUI sceneFile selected objects guiPickObjectID =
  void $ myWithWindow "Editor" do
    withMenuBarOpen do
      withMenuOpen "File" do
        whenM (menuItem "Save Scene") do
          writeFile sceneFile (show objects)

    let parented = HashMap.fromListWith (++) . mapMaybe (\(k,v) -> (,[k]) <$> v.baseObj) . HashMap.toList $ objects

    for_ (Map.toList objects) \(k, Object {baseObj}) -> when (isNothing baseObj || not (HashMap.member (fromMaybe (-1) baseObj) objects) ) do
      let children = fromMaybe [] $ HashMap.lookup k parented
      open <- treeNodeWith (pack $ show k)
        $   (if Map.member k selected then ImGuiTreeNodeFlags_Selected else zeroBits)
        .|. (if null children then ImGuiTreeNodeFlags_Leaf else zeroBits)

      whenM (isItemClicked (ImGuiMouseButton 0)) do
        guiPickObjectID k

      withDragDropTarget zeroBits "obj" \(droppedId :: Int) -> do
        pure ()
      withDragDropSource zeroBits "obj" k \_ -> do
        pure ()
      when open do
        for_ children \childId -> do
          childOpen <- treeNodeWith (pack $ show childId) (ImGuiTreeNodeFlags_Leaf .|. if Map.member childId selected then ImGuiTreeNodeFlags_Selected else zeroBits)

          whenM (isItemClicked (ImGuiMouseButton 0)) do
            guiPickObjectID childId

          when childOpen do
            treePop

        treePop
    pure Nothing
