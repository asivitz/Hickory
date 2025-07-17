{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Hickory.Graphics.DrawText
  ( textcommand
  , squareIndices
  ) where


import Hickory.Text.Types (TextCommand (..), XAlign (..), YAlign (..))

textcommand :: TextCommand
textcommand = TextCommand
  { text   = ""
  , align  = AlignCenter
  , valign = AlignMiddle
  , mScrollFrame = Nothing
  , mCursor = Nothing
  , mWrapWidth = Nothing
  }

squareIndices :: (Num a, Enum a, Ord a) => a -> ([a], a)
squareIndices numSquares = (indices, 6 * numSquares)
 where
  indices = concatMap (\((*4) -> i) -> [i, i+2, i+1, i+2, i+3, i+1])
    [0 .. (numSquares - 1)]
