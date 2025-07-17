{-# LANGUAGE DuplicateRecordFields #-}

module Hickory.Editor
  ( Attribute(..)
  , SomeAttribute(..)
  , glslStructDef
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
