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
import Data.Hashable (Hashable)
import Data.Proxy (Proxy (..))
import GHC.Word (Word32)

type Scalar = Double

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

data Attribute a where
  StringAttribute :: Attribute String
  FloatAttribute  :: Attribute Float
  IntAttribute    :: Attribute Int
  BoolAttribute   :: Attribute Bool

typeOfAttr :: forall a. Attribute a -> TypeRep a
typeOfAttr = \case
  StringAttribute -> typeRep
  FloatAttribute  -> typeRep
  IntAttribute    -> typeRep
  BoolAttribute   -> typeRep

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

instance Read (SomeAttribute Identity) where
  readPrec = lift do
    Lex.expect (Ident "SomeAttribute")
    let pars = between (skipSpaces >> string "(") (skipSpaces >> string ")")
    Lex.lex >>= \case
      Ident "StringAttribute" -> SomeAttribute StringAttribute <$> pars (readS_to_P (reads @(Identity String)))
      Ident "FloatAttribute"  -> SomeAttribute FloatAttribute  <$> pars (readS_to_P (reads @(Identity Float)))
      Ident "IntAttribute"    -> SomeAttribute IntAttribute    <$> pars (readS_to_P (reads @(Identity Int)))
      Ident "BoolAttribute"   -> SomeAttribute BoolAttribute   <$> pars (readS_to_P (reads @(Identity Bool)))
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
