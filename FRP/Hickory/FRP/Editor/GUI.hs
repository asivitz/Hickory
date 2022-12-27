{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}

module Hickory.FRP.Editor.GUI where

import qualified Reactive.Banana.Frameworks as B
import qualified Data.HashMap.Strict as Map
import DearImGui (withMenuBarOpen, withMenuOpen, menuItem, withCollapsingHeaderOpen, dragFloat3, colorEdit4, treePop, withDragDropTarget, acceptDragDropPayload, withDragDropSource, setDragDropPayload, isItemClicked, pattern ImGuiTreeNodeFlags_Selected, treeNodeWith, inputText, ImVec4 (..), checkbox, dragFloat, withComboOpen, selectable, button)
import Control.Monad.Extra (whenM)
import Data.Bits (zeroBits)
import Data.HashMap.Strict (HashMap)
import Hickory.FRP.Editor.Types
import Data.IORef (newIORef, modifyIORef', readIORef, IORef)
import Hickory.FRP.DearImGUIHelpers (myWithWindow)
import Data.Functor (void)
import Control.Monad (when)
import Data.Text (pack)
import Data.Foldable (for_, traverse_)
import Data.List (find, nub, delete)
import Data.Functor.Const (Const(..))
import Control.Lens (view)
import Data.Maybe (fromMaybe)
import Data.Traversable (for)

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

drawObjectEditorUI :: [Component] -> EditorState -> HashMap Int Object -> IO ()
drawObjectEditorUI componentDefs EditorState {..} objects = do
  myWithWindow "Object" do
    withCollapsingHeaderOpen "Transform" zeroBits do
      void $ dragFloat3 "Position" posRef 1 1 1
      void $ dragFloat3 "Scale" scaRef 1 1 1
      void $ dragFloat3 "Rotation" rotRef 1 1 1
    void $ colorEdit4 "Color" colorRef
    void $ inputText "Model" modelRef 100
    void $ inputText "Texture" textureRef 100
    void $ checkbox "Lit" litRef
    void $ checkbox "Casts Shadow" castsShadowRef
    void $ checkbox "Blend" blendRef
    void $ dragFloat "Specularity" specularityRef 1 0 1000

    readIORef componentsRef >>= traverse_ \comp ->
      withCollapsingHeaderOpen (pack comp) zeroBits do
        let Component {..} = fromMaybe (error "Can't find component def") $ find ((==comp) . view #name) componentDefs
        whenM (button "Delete") do
          modifyIORef' componentsRef (delete comp)
          -- for_ attributes \(SomeAttribute x) -> case x of
          --   FloatAttribute (Const attrName) -> do
          --     let ref :: IORef Float = case Map.lookup (comp, attrName) componentData of
          --                 Just (SomeAttribute (FloatAttribute r)) -> r
          --                 Nothing -> error "Can't find attribute ref"

        for_ attributes \(SomeAttribute x) -> case x of
          FloatAttribute (Const attrName) -> do
            let ref :: IORef Float = case Map.lookup (comp, attrName) componentData of
                        Just (SomeAttribute (FloatAttribute r)) -> r
                        Nothing -> error "Can't find attribute ref"
            void $ dragFloat (pack attrName) ref 1 0 1000
          _ -> pure ()

    withComboOpen "AddComponent" "Select" do
      for_ componentDefs \Component {..} ->
        selectable (pack name) >>= \case
          True -> modifyIORef' componentsRef (nub . (++ [name]))
          False -> pure ()

mkEditorState :: [Component] -> IO EditorState
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
  componentData <- Map.fromList . concat <$> for componentDefs \Component{..} ->
    for attributes \(SomeAttribute a) -> case a of
      StringAttribute (Const attrName) -> ((name, attrName),) . SomeAttribute . StringAttribute <$> newIORef ""
      FloatAttribute (Const attrName)  -> ((name, attrName),) . SomeAttribute . FloatAttribute  <$> newIORef 0
      IntAttribute (Const attrName)    -> ((name, attrName),) . SomeAttribute . IntAttribute    <$> newIORef 0
      BoolAttribute (Const attrName)   -> ((name, attrName),) . SomeAttribute . BoolAttribute   <$> newIORef False

  pure EditorState {..}
