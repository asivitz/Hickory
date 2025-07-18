{-# LANGUAGE DuplicateRecordFields #-}

module Hickory.Editor
  ( Object(..)
  , Attribute(..)
  , SomeAttribute(..)
  , glslStructDef
  , withAttrVal
  , pullAttrValMay
  , setSomeAttribute
  , mkSomeAttr
  , eqAttr
  , Attr
  ) where

import Hickory.Editor.Types
