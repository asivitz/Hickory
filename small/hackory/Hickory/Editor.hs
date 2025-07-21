{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module Hickory.Editor where

import Linear (M44, (^/), translation, V3(..), V4 (..), V2(..), identity, M33)
import DearImGui (ImVec4 (..))
import Data.IORef (IORef)
import GHC.Generics (Generic (..), M1 (..), K1 (..), S, Selector (..), (:*:) (..), C, D, U1 (..))
import Control.Lens (traversed, toListOf, (<&>))
import Data.Text (Text, pack, unpack)
import Data.Generics.Labels ()
import Data.HashMap.Strict (HashMap)
import Text.Read.Lex (Lexeme(..))
import qualified Text.Read.Lex as Lex
import GHC.Read (Read (..))
import Text.ParserCombinators.ReadPrec (lift)
import Text.ParserCombinators.ReadP (readS_to_P, between, string, skipSpaces)
import Data.Functor.Identity (Identity (..))
import Data.Functor.Const (Const(..))
import Type.Reflection (TypeRep, typeRep, eqTypeRep, type (:~~:) (..))
import qualified Data.HashMap.Strict as Map
import Data.Kind (Type)
import Hickory.ImGUI (imVec4ToV4, v4ToImVec4)
import Data.Hashable (Hashable)
import Data.Proxy (Proxy (..))
import GHC.Word (Word32)

isThisEnough = do
  print (ImVec4 1 1 1 1)

{- Generic GLSL struct definitions -}

class GlslType a where
  glslTypeName :: Proxy a -> String

instance GlslType (V4 Float)  where glslTypeName _ = "vec4"

class GHasGlslUniformDef (f :: Type -> Type) where
  gGlslLines :: Proxy f -> [String]

instance GHasGlslUniformDef U1 where
  gGlslLines _ = []

instance GHasGlslUniformDef f => GHasGlslUniformDef (M1 D x f) where
  gGlslLines _ = gGlslLines (Proxy :: Proxy f)

instance GHasGlslUniformDef f => GHasGlslUniformDef (M1 C x f) where
  gGlslLines _ = gGlslLines (Proxy :: Proxy f)

instance (GHasGlslUniformDef f, GHasGlslUniformDef g)
      => GHasGlslUniformDef (f :*: g) where
  gGlslLines _ =
    gGlslLines (Proxy :: Proxy f) ++
    gGlslLines (Proxy :: Proxy g)

instance (Selector s, GlslType a)
      => GHasGlslUniformDef (M1 S s (K1 i a)) where
  gGlslLines _ =
    let fieldName = selName (undefined :: M1 S s (K1 i a) p)
        glslType  = glslTypeName (Proxy @a)
    in [ "  " <> glslType <> " " <> fieldName <> ";" ]

class (Generic t, GHasGlslUniformDef (Rep t)) => HasGlslUniformDef t where
  glslUniformLines :: [String]
  default glslUniformLines :: (Generic t, GHasGlslUniformDef (Rep t)) => [String]
  glslUniformLines = gGlslLines (Proxy :: Proxy (Rep t))

  glslStructDef :: String
  glslStructDef =
    "struct " <> "Uniforms" <> "\n{\n"
    <> unlines (glslUniformLines @t)
    <> "};\n"

instance (Generic t, GHasGlslUniformDef (Rep t)) => HasGlslUniformDef t
