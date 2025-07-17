{-# LANGUAGE FlexibleContexts, OverloadedRecordDot #-}

module Hickory.Editor.View where

import Hickory.Math (mkScale, mkRotation, mkTranslation, Scalar, v2angle)
import Hickory.Types (Size (..))
import Linear (V3 (..), V2 (..), (!*!), _x, _y, _z, V4 (..), norm, normalize, (^*), unit, zero, _m33, inv33, transpose)
import Data.Fixed (div')
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import Hickory.Editor.Types
import Control.Lens ((.~), (&), (^.))
import Control.Monad (when)
import Data.Foldable (for_)
import Foreign (poke)
import Data.Maybe (mapMaybe)
import GHC.Word (Word32)
