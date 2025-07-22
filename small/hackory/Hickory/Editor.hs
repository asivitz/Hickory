{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}

module Hickory.Editor where

import Linear (V4(..))
import DearImGui (ImVec4 (..))
import GHC.Generics (Generic (..), M1 (..), K1 (..), S, Selector (..), C, D, U1 (..))
import Data.Generics.Labels ()
import Data.Kind (Type)
import Hickory.ImGUI ()
import Data.Proxy (Proxy (..))

isTheyEqual :: ImVec4 -> ImVec4 -> Bool
isTheyEqual = (==)

class GHasGlslUniformDef (f :: Type -> Type) where
  gGlslLines :: Proxy f -> [String]

instance GHasGlslUniformDef U1 where
  gGlslLines _ = []

instance GHasGlslUniformDef f => GHasGlslUniformDef (M1 D x f) where
  gGlslLines _ = gGlslLines (Proxy :: Proxy f)

instance GHasGlslUniformDef f => GHasGlslUniformDef (M1 C x f) where
  gGlslLines _ = gGlslLines (Proxy :: Proxy f)

instance (Selector s)
      => GHasGlslUniformDef (M1 S s (K1 i a)) where
  gGlslLines _ =
    let fieldName = selName (undefined :: M1 S s (K1 i a) p)
    in [ "  " <> fieldName <> ";" ]
