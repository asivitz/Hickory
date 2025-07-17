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

module Hickory.Editor.Types where

import Linear (M44, (^/), translation, V3(..), V4 (..), V2(..), identity, M33)
import DearImGui (ImVec4 (..))
import Data.IORef (IORef)
import GHC.Generics (Generic (..), M1 (..), K1 (..), S, Selector (..), (:*:) (..), C, D, U1 (..))
import Control.Lens (traversed, toListOf, (<&>))
import Hickory.Math (Scalar)
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
import Hickory.ImGUI.Helpers (v3ToTriple, tripleToV3, tupleToV2, v2ToTuple, imVec4ToV4, v4ToImVec4)
import Data.Hashable (Hashable)
import Hickory.Vulkan.Types (VulkanResources)
import Data.Proxy (Proxy (..))
import GHC.Word (Word32)

data ObjectManipMode = OTranslate | OScale | ORotate
  deriving Eq

data Object = Object
  { transform   :: M44 Scalar
  -- List of component names and attribute maps
  , components  :: [(String, HashMap String (SomeAttribute Identity))]
  , baseObj :: Maybe Word32
  } deriving (Generic, Show, Read)

-- Types which have an 'attribute' representation in the editor
class Attr a where
  mkAttr :: Attribute a

  -- Sometimes the storage type differs (e.g. DearIMGui represents Vec3s as a float triple)
  type AttrRef a :: Type
  type AttrRef a = a

instance Attr String where
  mkAttr = StringAttribute
  type AttrRef String = Text
instance Attr Float  where mkAttr = FloatAttribute
instance Attr Int    where mkAttr = IntAttribute
instance Attr Bool   where mkAttr = BoolAttribute
instance Attr (V3 Scalar) where
  mkAttr = V3Attribute
  type AttrRef (V3 Scalar) = (Float, Float, Float)
instance Attr (V2 Scalar) where
  mkAttr = V2Attribute
  type AttrRef (V2 Scalar) = (Float, Float)
instance Attr (V4 Scalar) where
  mkAttr = ColorAttribute
  type AttrRef (V4 Scalar) = ImVec4
instance Attr (M33 Scalar) where
  mkAttr = Mat3Attribute
  type AttrRef (M33 Scalar) = Scalar -- TODO
instance Attr (M44 Scalar) where
  mkAttr = Mat4Attribute
  type AttrRef (M44 Scalar) = Scalar -- TODO

data Attribute a where
  StringAttribute :: Attribute String
  FloatAttribute  :: Attribute Float
  IntAttribute    :: Attribute Int
  BoolAttribute   :: Attribute Bool
  V3Attribute     :: Attribute (V3 Scalar)
  V2Attribute     :: Attribute (V2 Scalar)
  ColorAttribute  :: Attribute (V4 Scalar)
  Mat3Attribute   :: Attribute (M33 Scalar)
  Mat4Attribute   :: Attribute (M44 Scalar)

typeOfAttr :: forall a. Attribute a -> TypeRep a
typeOfAttr = \case
  StringAttribute -> typeRep
  FloatAttribute  -> typeRep
  IntAttribute    -> typeRep
  BoolAttribute   -> typeRep
  V3Attribute     -> typeRep
  V2Attribute     -> typeRep
  ColorAttribute  -> typeRep
  Mat3Attribute   -> typeRep
  Mat4Attribute   -> typeRep

data AttrClasses a where
  -- Provides proof of a type having certain instances
  AttrClasses :: (Eq a, Eq (AttrRef a)) => AttrClasses a

-- Prove that each attribute has some necessary instances
proveAttrClasses :: Attribute a -> AttrClasses a
proveAttrClasses = \case
  StringAttribute -> AttrClasses
  FloatAttribute  -> AttrClasses
  IntAttribute    -> AttrClasses
  BoolAttribute   -> AttrClasses
  V3Attribute     -> AttrClasses
  V2Attribute     -> AttrClasses
  ColorAttribute  -> AttrClasses
  Mat3Attribute   -> AttrClasses
  Mat4Attribute   -> AttrClasses

-- Check if two attributes have the same type
eqAttr :: Attribute a1 -> Attribute a2 -> Maybe (a1 :~~: a2)
eqAttr a b = eqTypeRep (typeOfAttr a) (typeOfAttr b)

data SomeAttribute contents = forall a. Attr a => SomeAttribute    { attr :: Attribute a, contents :: contents a }

instance Show (SomeAttribute Identity) where
  show (SomeAttribute attr val) = "SomeAttribute " ++ case attr of
    StringAttribute -> "StringAttribute (" ++ show val ++ ")"
    FloatAttribute  -> "FloatAttribute ("  ++ show val ++ ")"
    IntAttribute    -> "IntAttribute ("    ++ show val ++ ")"
    BoolAttribute   -> "BoolAttribute ("   ++ show val ++ ")"
    V3Attribute     -> "V3Attribute ("   ++ show val ++ ")"
    V2Attribute     -> "V2Attribute ("   ++ show val ++ ")"
    ColorAttribute  -> "ColorAttribute ("   ++ show val ++ ")"
    Mat3Attribute   -> "Mat3Attribute ("   ++ show val ++ ")"
    Mat4Attribute   -> "Mat4Attribute ("   ++ show val ++ ")"

instance Read (SomeAttribute Identity) where
  readPrec = lift do
    Lex.expect (Ident "SomeAttribute")
    let pars = between (skipSpaces >> string "(") (skipSpaces >> string ")")
    Lex.lex >>= \case
      Ident "StringAttribute" -> SomeAttribute StringAttribute <$> pars (readS_to_P (reads @(Identity String)))
      Ident "FloatAttribute"  -> SomeAttribute FloatAttribute  <$> pars (readS_to_P (reads @(Identity Float)))
      Ident "IntAttribute"    -> SomeAttribute IntAttribute    <$> pars (readS_to_P (reads @(Identity Int)))
      Ident "BoolAttribute"   -> SomeAttribute BoolAttribute   <$> pars (readS_to_P (reads @(Identity Bool)))
      Ident "V3Attribute"     -> SomeAttribute V3Attribute     <$> pars (readS_to_P (reads @(Identity (V3 Scalar))))
      Ident "V2Attribute"     -> SomeAttribute V2Attribute     <$> pars (readS_to_P (reads @(Identity (V2 Scalar))))
      Ident "ColorAttribute"  -> SomeAttribute ColorAttribute  <$> pars (readS_to_P (reads @(Identity (V4 Scalar))))
      Ident "Mat3Attribute"   -> SomeAttribute Mat3Attribute   <$> pars (readS_to_P (reads @(Identity (M33 Scalar))))
      Ident "Mat4Attribute"   -> SomeAttribute Mat4Attribute   <$> pars (readS_to_P (reads @(Identity (M44 Scalar))))
      _ -> fail "Invalid attribute type"

-- Look up the value for an attribute
withAttrVal :: forall a b k. (Attr a, Hashable k) => HashMap k (SomeAttribute Identity) -> k -> (a -> b) -> b
withAttrVal attrs name f = case Map.lookup name attrs of
  Just (SomeAttribute attr (Identity v)) -> case eqAttr attr (mkAttr :: Attribute a) of
    Just HRefl -> f v
    Nothing -> error "Wrong type for attribute"
  Nothing -> f $ defaultAttrVal (mkAttr :: Attribute a)

pullAttrValMay :: forall a. Attr a => SomeAttribute Identity -> Maybe a
pullAttrValMay = \case
  SomeAttribute attr (Identity v) -> case eqAttr attr (mkAttr :: Attribute a) of
    Just HRefl -> Just v
    Nothing -> Nothing

setSomeAttribute :: forall f a. Attr a => f a -> SomeAttribute f -> SomeAttribute f
setSomeAttribute newV (SomeAttribute attr _) = case eqAttr attr (mkAttr :: Attribute a) of
    Just HRefl -> SomeAttribute attr newV
    Nothing -> error "Wrong type for attribute"


mkSomeAttr :: forall a. Attr a => Proxy a -> String -> SomeAttribute (Const String)
mkSomeAttr _ name = SomeAttribute (mkAttr :: Attribute a) (Const name)

defaultAttrVal :: Attribute a -> a
defaultAttrVal = \case
  StringAttribute -> ""
  FloatAttribute -> 0
  IntAttribute -> 0
  BoolAttribute -> False
  V3Attribute -> V3 1 0 0
  V2Attribute -> V2 1 0
  ColorAttribute -> V4 1 1 1 1
  Mat3Attribute -> identity
  Mat4Attribute -> identity

mkDefaultComponent :: [SomeAttribute (Const String)] -> HashMap String (SomeAttribute Identity)
mkDefaultComponent xs = Map.fromList $ xs <&> \case
  SomeAttribute attr (Const name) -> (name, SomeAttribute attr (Identity $ defaultAttrVal attr))

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

instance GRecordAttributes f => GRecordAttributes (M1 C x f) where
  gToAttributeList _ = gToAttributeList (Proxy :: Proxy f)
  gFromHashMap hm = M1 (gFromHashMap hm)

instance (GRecordAttributes f, GRecordAttributes g)
      => GRecordAttributes (f :*: g) where
  gToAttributeList _ =
    gToAttributeList (Proxy :: Proxy f) ++ gToAttributeList (Proxy :: Proxy g)
  gFromHashMap hm =
    let left  = gFromHashMap hm
        right = gFromHashMap hm
    in left :*: right

-- A single field
instance (Selector s, Attr a) => GRecordAttributes (M1 S s (K1 i a)) where
  gToAttributeList _ =
    let fieldName = selName (undefined :: M1 S s (K1 i a) p)
     in [ SomeAttribute (mkAttr @a) (Const fieldName) ]
  gFromHashMap hm =
    let fieldName = selName (undefined :: M1 S s (K1 i a) p)
        val = withAttrVal hm fieldName id
    in M1 (K1 val)

-- User-facing class
class (Generic comp, GRecordAttributes (Rep comp))
   => HasRecordAttributes comp where
  toAttributeList :: [SomeAttribute (Const String)]
  toAttributeList = gToAttributeList (Proxy :: Proxy (Rep comp))

  fromHashMap :: HashMap String (SomeAttribute Identity) -> comp
  fromHashMap hm = to (gFromHashMap hm)

instance (Generic comp, GRecordAttributes (Rep comp))
      => HasRecordAttributes comp

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

{- Helpers -}

avg :: [V3 Scalar] -> V3 Scalar
avg vs = sum vs ^/ (fromIntegral $ length vs)

avgObjTranslation :: Traversable t => t Object -> V3 Scalar
avgObjTranslation objs = avg $ toListOf (traversed . #transform . translation) objs
