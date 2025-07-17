{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module SmallDep.Foo where

import Data.String.QM (qm)
import GHC.Generics
import Data.Proxy (Proxy (..))
import Type.Reflection (TypeRep, typeRep, eqTypeRep, type (:~~:) (..))
import Data.Kind (Type)

class GlslType a where
  glslTypeName :: Proxy a -> String

instance GlslType Bool        where glslTypeName _ = "bool"
instance GlslType Float       where glslTypeName _ = "float"

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
