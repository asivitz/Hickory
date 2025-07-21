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
import Type.Reflection (TypeRep, typeRep, eqTypeRep, type (:~~:) (..))
import Data.Kind (Type)
import Hickory.ImGUI ()
import Data.Proxy (Proxy (..))

class Attr a where
  mkAttr :: Attribute a

  type AttrRef a :: Type
  type AttrRef a = a

instance Attr (V4 Double) where
  mkAttr = ColorAttribute
  type AttrRef (V4 Double) = ImVec4

data Attribute a where
  ColorAttribute  :: Attribute (V4 Double)

typeOfAttr :: forall a. Attribute a -> TypeRep a
typeOfAttr = \case
  ColorAttribute  -> typeRep

data AttrClasses a where
  AttrClasses :: (Eq a, Eq (AttrRef a)) => AttrClasses a

proveAttrClasses :: Attribute a -> AttrClasses a
proveAttrClasses = \case
  ColorAttribute  -> AttrClasses

eqAttr :: Attribute a1 -> Attribute a2 -> Maybe (a1 :~~: a2)
eqAttr a b = eqTypeRep (typeOfAttr a) (typeOfAttr b)

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

instance (Selector s, GlslType a)
      => GHasGlslUniformDef (M1 S s (K1 i a)) where
  gGlslLines _ =
    let fieldName = selName (undefined :: M1 S s (K1 i a) p)
        glslType  = glslTypeName (Proxy @a)
    in [ "  " <> glslType <> " " <> fieldName <> ";" ]

class (Generic t, GHasGlslUniformDef (Rep t)) => HasGlslUniformDef t where
  glslUniformLines :: [String]
  glslUniformLines = gGlslLines (Proxy :: Proxy (Rep t))

  glslStructDef :: String
  glslStructDef =
    "struct " <> "Uniforms" <> "\n{\n"
    <> unlines (glslUniformLines @t)
    <> "};\n"

instance (Generic t, GHasGlslUniformDef (Rep t)) => HasGlslUniformDef t
