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

import Linear (M44, V3(..), V4 (..), V2(..), identity, M33)
import DearImGui (ImVec4 (..))
import GHC.Generics (Generic (..), M1 (..), K1 (..), S, Selector (..), (:*:) (..), C, D, U1 (..))
import Data.Text (Text)
import Data.Kind (Type)
import Hickory.ImGUI ()
import Data.Proxy (Proxy (..))

type Scalar = Double

{- Generic GLSL struct definitions -}

class GlslType a where
  glslTypeName :: Proxy a -> String

instance GlslType Bool        where glslTypeName _ = "bool"
instance GlslType Float       where glslTypeName _ = "float"
instance GlslType (V2 Float)  where glslTypeName _ = "vec2"
instance GlslType (V3 Float)  where glslTypeName _ = "vec3"
instance GlslType (V4 Float)  where glslTypeName _ = "vec4"
instance GlslType (M33 Float) where glslTypeName _ = "mat3"
instance GlslType (M44 Float) where glslTypeName _ = "mat4"

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
