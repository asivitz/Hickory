{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Hickory.FRP.Editor.GUI where

import qualified Reactive.Banana.Frameworks as B
import qualified Data.HashMap.Strict as Map
import DearImGui (withMenuBarOpen, withMenuOpen, menuItem, withCollapsingHeaderOpen, dragFloat3, colorEdit4, treePop, withDragDropTarget, acceptDragDropPayload, withDragDropSource, setDragDropPayload, isItemClicked, pattern ImGuiTreeNodeFlags_Selected, treeNodeWith, inputText, checkbox, dragFloat, withComboOpen, selectable, button, dragInt, dragFloat2, pattern ImGuiTreeNodeFlags_Leaf)
import Hickory.ImGUI.Helpers (myWithWindow, v3ToTriple, tripleToV3, v2ToTuple, tupleToV2, imVec4ToV4, v4ToImVec4)
import Control.Monad.Extra (whenM)
import Data.Bits (zeroBits, (.|.))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Hickory.FRP.Editor.Types
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
import Hickory.FRP.Editor.General (setScale, setRotation, matScale, matEuler)
import Text.Printf (printf)
import Hickory.Utils.Utils (deleteAt)
import Data.Word (Word32)

drawMainEditorUI :: FilePath -> HashMap Word32 Object -> HashMap Word32 Object -> B.Handler Word32 -> IO ()
drawMainEditorUI sceneFile selected objects guiPickObjectID =
  myWithWindow "Editor" do
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
        for_ children \childId -> do
          childOpen <- treeNodeWith (pack $ show childId) (ImGuiTreeNodeFlags_Leaf .|. if Map.member childId selected then ImGuiTreeNodeFlags_Selected else zeroBits)

          whenM isItemClicked do
            guiPickObjectID childId

          when childOpen do
            treePop

        treePop

drawObjectEditorUI :: HashMap String (Component m a) -> IORef (HashMap Word32 Object) -> [Word32] -> IO ()
drawObjectEditorUI componentDefs objectsRef selectedIds = do
  objs <- readIORef objectsRef
  for_ (headMay selectedIds) \representativeId -> do
    let
      oneSelected = HashMap.lookup representativeId objs
      mkVar :: Traversal' Object a -> (a -> b) -> (b -> a -> a) -> b -> StateVar b
      mkVar l tg ts def = makeStateVar
                        (fromMaybe def . preview (ix representativeId . l . to tg) <$> readIORef objectsRef)
                        (modifyIORef' objectsRef . (\v m -> foldl' (\b a -> over (ix a . l) v b) m selectedIds) . ts)
      mkComponentVar :: forall a b. Attr a => Int -> String -> (a -> b) -> (b -> a) -> StateVar b
      mkComponentVar i attrName g s = mkVar (#components . ix i . _2 . at attrName)
                                            (g . maybe (defaultAttrVal (mkAttr :: Attribute a)) pullAttrVal)
                                            (const . Just . SomeAttribute (mkAttr :: Attribute a) . Identity . s)
                                            (g $ defaultAttrVal (mkAttr :: Attribute a))

    myWithWindow "Object" do
      withCollapsingHeaderOpen "Transform" zeroBits do
        void $ dragFloat3 "Position" (mkVar (#transform . translation) v3ToTriple (const . tripleToV3) (0,0,0))  1 1 1
        void $ dragFloat3 "Scale" (mkVar #transform (v3ToTriple . matScale) (setScale . tripleToV3) (0,0,0)) 1 1 1
        void $ dragFloat3 "Rotation" (mkVar #transform (v3ToTriple . matEuler) (setRotation . tripleToV3) (0,0,0)) 1 1 1

      ifor_ (maybe [] (.components) oneSelected) \i (compName, _attrVals) -> do
        withCollapsingHeaderOpen (pack $ printf "[%d] %s" i compName) zeroBits do
          whenM (button "Delete") do
            modifyIORef' objectsRef $ \m -> foldl' (\b a -> over (ix a . #components) (`deleteAt` i) b) m selectedIds
          let compDef = HashMap.lookup compName componentDefs
          for_ (maybe [] (.attributes) compDef) \defSa -> do
            case defSa of
              SomeAttribute attr (Const attrName) -> do
                () <- case attr of
                  FloatAttribute  -> void $ dragFloat (pack attrName) (mkComponentVar i attrName id id) 1 0 200000
                  IntAttribute    -> void $ dragInt   (pack attrName) (mkComponentVar i attrName id id ) 1 0 200000
                  StringAttribute -> void $ inputText (pack attrName) (mkComponentVar i attrName pack unpack) 30
                  BoolAttribute   -> void $ checkbox (pack attrName) (mkComponentVar i attrName id id)
                  V3Attribute     -> void $ dragFloat3 (pack attrName) (mkComponentVar i attrName v3ToTriple tripleToV3) 1 1 1
                  V2Attribute     -> void $ dragFloat2 (pack attrName) (mkComponentVar i attrName v2ToTuple tupleToV2) 1 1 1
                  ColorAttribute  -> void $ colorEdit4 (pack attrName) (mkComponentVar i attrName v4ToImVec4 imVec4ToV4)
                pure ()

      withComboOpen "AddComponent" "Select" do
        for_ (Map.toList componentDefs) \(name, def) ->
          selectable (pack name) >>= \case
            True -> modifyIORef' objectsRef $ \m -> foldl' (\b a -> over (ix a . #components) (++ [(name, mkDefaultComponent def.attributes)]) b) m selectedIds
            False -> pure ()
  where
  -- Look up the value for an attribute
  pullAttrVal :: forall a. Attr a => SomeAttribute Identity -> a
  pullAttrVal = \case
    SomeAttribute attr (Identity v) -> case eqAttr attr (mkAttr :: Attribute a) of
      Just HRefl -> v
      Nothing -> error "Wrong type for attribute"

  setAttrVal :: forall a. Attr a => a -> SomeAttribute Identity -> SomeAttribute Identity
  setAttrVal = setSomeAttribute . Identity
