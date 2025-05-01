{-# LANGUAGE DuplicateRecordFields #-}

module Hickory.FRP.Editor
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

import Hickory.FRP.Editor.Network
import Hickory.FRP.Editor.Post
import Hickory.FRP.Editor.Types
import Hickory.FRP.Editor.View
