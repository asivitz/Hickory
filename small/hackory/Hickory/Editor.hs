{-# LANGUAGE DuplicateRecordFields #-}

module Hickory.Editor
  ( editorScene
  , editorLayer
  , drawPostUI
  , GraphicsParams(..)
  , Object(..)
  , drawObjects
  , defaultGraphicsParams
  , Component(..)
  , Attribute(..)
  , SomeAttribute(..)
  , glslStructDef
  , mkComponent
  , mkComponent2
  , mkComponent3
  , mkComponent4
  , mkComponent5
  , mkComponent6
  , mkComponent7
  , withAttrVal
  , pullAttrValMay
  , setSomeAttribute
  , mkSomeAttr
  , eqAttr
  , Attr
  ) where

import Hickory.Editor.Network
import Hickory.Editor.Post
import Hickory.Editor.Types
import Hickory.Editor.View
