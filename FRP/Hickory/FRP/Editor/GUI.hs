{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Hickory.FRP.Editor.GUI where

import qualified Reactive.Banana.Frameworks as B
import qualified Data.HashMap.Strict as Map
import DearImGui (withMenuBarOpen, withMenuOpen, menuItem, withCollapsingHeaderOpen, dragFloat3, colorEdit4, treePop, withDragDropTarget, acceptDragDropPayload, withDragDropSource, setDragDropPayload, isItemClicked, pattern ImGuiTreeNodeFlags_Selected, treeNodeWith, inputText, ImVec4 (..), checkbox, dragFloat, withComboOpen, selectable, button, dragInt, dragFloat2)
import Control.Monad.Extra (whenM)
import Data.Bits (zeroBits)
import Data.HashMap.Strict (HashMap)
import Hickory.FRP.Editor.Types
import Data.IORef (newIORef, modifyIORef', readIORef, writeIORef)
import Hickory.FRP.DearImGUIHelpers (myWithWindow)
import Data.Functor (void)
import Control.Monad (when)
import Data.Text (pack)
import Data.Foldable (for_, traverse_)
import Data.List (nub, delete)
import Data.Functor.Const (Const(..))
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Type.Reflection (type (:~~:) (..))

drawMainEditorUI :: EditorState -> FilePath -> HashMap Int Object -> HashMap Int Object -> B.Handler Int -> IO ()
drawMainEditorUI EditorState {..} sceneFile selected objects guiPickObjectID =
  myWithWindow "Editor" do
    withMenuBarOpen do
      withMenuOpen "File" do
        whenM (menuItem "Save Scene") do
          writeFile sceneFile (show objects)

    for_ (Map.toList objects) \(k, Object {}) -> do
      open <- treeNodeWith (pack $ show k) (if Map.member k selected then ImGuiTreeNodeFlags_Selected else zeroBits)

      whenM isItemClicked do
        guiPickObjectID k

      withDragDropTarget do
        acceptDragDropPayload "obj" >>= \case
          Nothing -> pure ()
          Just (droppedId :: Int) -> pure ()
      withDragDropSource do
        _ <- setDragDropPayload "obj" k
        pure ()
      when open do
        treePop

drawObjectEditorUI :: HashMap String (Component m a) -> EditorState -> HashMap Int Object -> IO ()
drawObjectEditorUI componentDefs EditorState {..} objects = do
  myWithWindow "Object" do
    withCollapsingHeaderOpen "Transform" zeroBits do
      void $ dragFloat3 "Position" posRef 1 1 1
      void $ dragFloat3 "Scale" scaRef 1 1 1
      void $ dragFloat3 "Rotation" rotRef 1 1 1

    readIORef componentsRef >>= traverse_ \comp ->
      withCollapsingHeaderOpen (pack comp) zeroBits do
        let Component {..} = fromMaybe (error "Can't find component def") $ Map.lookup comp componentDefs
        whenM (button "Delete") do
          modifyIORef' componentsRef (delete comp)
          for_ attributes \(SomeAttribute _attr (Const attrName)) ->
            case Map.lookup (comp, attrName) componentData of
              Nothing -> error "Can't find attribute ref" :: IO ()
              Just (SomeAttributeRef attr' ref) -> writeIORef ref (toAttrRefType $ defaultAttrVal attr')
        for_ attributes \(SomeAttribute attr (Const attrName)) ->
          case Map.lookup (comp, attrName) componentData of
            Nothing -> error "Can't find attribute ref" :: IO ()
            Just (SomeAttributeRef attr' ref) -> case eqAttr attr attr' of
              Just HRefl -> case attr of
                FloatAttribute  -> void $ dragFloat (pack attrName) ref 1 0 200000
                IntAttribute    -> void $ dragInt   (pack attrName) ref 1 0 200000
                StringAttribute -> void $ inputText (pack attrName) ref 30
                BoolAttribute   -> void $ checkbox (pack attrName) ref
                V3Attribute     -> void $ dragFloat3 (pack attrName) ref 1 1 1
                V2Attribute     -> void $ dragFloat2 (pack attrName) ref 1 1 1
                ColorAttribute  -> void $ colorEdit4 (pack attrName) ref
              Nothing -> error "Attribute types don't match"

    withComboOpen "AddComponent" "Select" do
      for_ (Map.keys componentDefs) \name ->
        selectable (pack name) >>= \case
          True -> modifyIORef' componentsRef (nub . (++ [name]))
          False -> pure ()

mkEditorState :: HashMap String (Component m a) -> IO EditorState
mkEditorState componentDefs = do
  posRef <- newIORef (0,0,0)
  rotRef <- newIORef (0,0,0)
  scaRef <- newIORef (0,0,0)
  colorRef <- newIORef (ImVec4 1 1 1 1)
  modelRef <- newIORef ""
  textureRef <- newIORef ""
  litRef <- newIORef False
  castsShadowRef <- newIORef False
  blendRef <- newIORef False
  specularityRef <- newIORef 8
  componentsRef <- newIORef []
  componentData <- Map.fromList . concat <$> for (Map.toList componentDefs) \(name, Component{..}) ->
    for attributes \(SomeAttribute attr (Const attrName)) ->
      (\x -> ((name, attrName), SomeAttributeRef attr x)) <$> newIORef (toAttrRefType $ defaultAttrVal attr)

  pure EditorState {..}
