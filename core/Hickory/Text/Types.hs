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
  , mScrollFrame :: Maybe (Float, Float) -- vertical cutoffs 'from' and 'to', in number of lines
  , mCursor      :: Maybe (Word, Word)
  , mWrapWidth   :: Maybe Float
  }
  deriving (Show, Eq, Generic)
  deriving anyclass Hashable
