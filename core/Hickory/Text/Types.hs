{-# LANGUAGE NamedFieldPuns, DerivingStrategies, DeriveAnyClass, DeriveGeneric #-}

module Hickory.Text.Types where

import Data.Hashable (Hashable (..))
import GHC.Generics (Generic)
import qualified Data.Text as Text

data XAlign
  = AlignRight
  | AlignCenter
  | AlignLeft
  deriving (Show, Eq, Generic)
  deriving anyclass Hashable

data YAlign
  = AlignMiddle
  | AlignBottom
  | AlignTop
  deriving (Show, Eq, Generic)
  deriving anyclass Hashable

data TextCommand = TextCommand
  { text     :: Text.Text
  , align    :: XAlign
  , valign   :: YAlign
  }
  deriving (Show, Eq, Generic)
  deriving anyclass Hashable
