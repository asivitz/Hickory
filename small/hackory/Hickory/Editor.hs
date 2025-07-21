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

import Linear (V4(..))
import DearImGui (ImVec4 (..))
import GHC.Generics (Generic (..), M1 (..), K1 (..), S, Selector (..), C, D, U1 (..))
import Data.Generics.Labels ()
import Data.HashMap.Strict (HashMap)
import Data.Functor.Identity (Identity (..))
import Data.Functor.Const (Const(..))
import Type.Reflection (TypeRep, typeRep, eqTypeRep, type (:~~:) (..))
import qualified Data.HashMap.Strict as Map
import Data.Kind (Type)
import Hickory.ImGUI ()
import Data.Hashable (Hashable)
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

eqAttr :: Attribute a1 -> Attribute a2 -> Maybe (a1 :~~: a2)
eqAttr a b = eqTypeRep (typeOfAttr a) (typeOfAttr b)

data SomeAttribute contents = forall a. Attr a => SomeAttribute    { attr :: Attribute a, contents :: contents a }

-- Look up the value for an attribute
withAttrVal :: forall a b k. (Attr a, Hashable k) => HashMap k (SomeAttribute Identity) -> k -> (a -> b) -> b
withAttrVal attrs name f = case Map.lookup name attrs of
  Just (SomeAttribute attr (Identity v)) -> case eqAttr attr (mkAttr :: Attribute a) of
    Just HRefl -> f v
    Nothing -> error "Wrong type for attribute"
  Nothing -> f $ defaultAttrVal (mkAttr :: Attribute a)

defaultAttrVal :: Attribute a -> a
defaultAttrVal = \case
  ColorAttribute -> V4 1 1 1 1

{- Generics -}

class GRecordAttributes (f :: Type -> Type) where
  gToAttributeList :: Proxy f -> [SomeAttribute (Const String)]
  gFromHashMap :: HashMap String (SomeAttribute Identity) -> f p

-- Empty constructor
instance GRecordAttributes U1 where
  gToAttributeList _ = []
  gFromHashMap _ = U1

-- For metadata that doesn't affect structure (the D and C metadata),
-- we just pass through to the contained representation.
instance GRecordAttributes f => GRecordAttributes (M1 D x f) where
  gToAttributeList _ = gToAttributeList (Proxy :: Proxy f)
  gFromHashMap hm = M1 (gFromHashMap hm)

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
