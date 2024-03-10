{-# LANGUAGE PatternSynonyms #-}

module Hickory.Vulkan.Renderer.Stats where

import DearImGui (withMenuBarOpen, withMenuOpen, menuItem, withCollapsingHeaderOpen, dragFloat3, colorEdit4, treePop, withDragDropTarget, acceptDragDropPayload, withDragDropSource, setDragDropPayload, isItemClicked, pattern ImGuiTreeNodeFlags_Selected, treeNodeWith, inputText, ImVec4 (..), checkbox, dragFloat, withComboOpen, selectable, button, dragInt, dragFloat2, pattern ImGuiTreeNodeFlags_Leaf, text)
import Hickory.DearImGUIHelpers (myWithWindow, v3ToTriple, tripleToV3, v2ToTuple, tupleToV2, imVec4ToV4, v4ToImVec4)
import Control.Lens (ifor_)
import Text.Printf (printf)
import Data.Text (pack)

data Stats = Stats
  { numLitDraws :: Int
  , numGBuffer :: Int
  , numGBufferInstances :: Int
  , numGBufferPostCullInstances :: Int
  , numCastingShadows :: Int
  , numInstancesPerCascade :: [Int]
  , numDirect :: Int
  , numOverlayDraws :: Int
  , logMessages :: String
  }

drawStats :: Stats -> IO ()
drawStats Stats {..} =
  myWithWindow "Renderer" do
    text . pack $ printf "Total lit draw commands: %d" numLitDraws
    text . pack $ printf "Number commands targeted to gbuffer: %d" numGBuffer
    text . pack $ printf "Number instances targeted to gbuffer: %d" numGBufferInstances
    text . pack $ printf "Number instances hitting gbuffer: %d" numGBufferPostCullInstances
    text . pack $ printf "Number commands casting shadows: %d" numCastingShadows
    ifor_ numInstancesPerCascade \i n ->
      text . pack $ printf "Number instances hitting cascade %d: %d" i n
    text . pack $ printf "Number commands direct: %d" numDirect
    text . pack $ printf "Total overlay draw commands: %d" numOverlayDraws
    text . pack $ logMessages
