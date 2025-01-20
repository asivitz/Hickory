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

module Hickory.FRP.Editor.Types where

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
import Hickory.Resources (ResourcesStore)
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

data Component m a = Component
  { attributes :: [SomeAttribute (Const String)]
  , acquire    :: HashMap String (SomeAttribute Identity) -> VulkanResources -> ResourcesStore -> IO ()
  , draw       :: HashMap String (SomeAttribute Identity)
               -> Maybe a
               -> [(Word32, M44 Scalar)] -- ObjectId and Transform per instance
               -> m ()
  }

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
-- Need a separate type for Refs because unsaturated type families can't be used as arguments
data SomeAttributeRef       = forall a. Attr a => SomeAttributeRef { attr :: Attribute a, contents :: IORef (AttrRef a) }

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

mkComponent2 :: forall a b m state. (Attr a, Attr b) => String -> String -> (a -> b -> VulkanResources -> ResourcesStore -> IO ()) -> (a -> b -> Maybe state -> [(Word32,M44 Scalar)] -> m ()) -> Component m state
mkComponent2 arg1 arg2 acquire f = Component
  [ SomeAttribute (mkAttr :: Attribute a) (Const arg1)
  , SomeAttribute (mkAttr :: Attribute b) (Const arg2)
  ]
  (\attrs -> withAttrVal attrs arg1 \v1 ->
             withAttrVal attrs arg2 \v2 -> acquire v1 v2)
  (\attrs state -> withAttrVal attrs arg1 \v1 ->
                   withAttrVal attrs arg2 \v2 -> f v1 v2 state)

mkComponent3 :: forall a b c m state. (Attr a, Attr b, Attr c) => String -> String -> String -> (a -> b -> c -> VulkanResources -> ResourcesStore -> IO ()) -> (a -> b -> c -> Maybe state -> [(Word32, M44 Scalar)] -> m ()) -> Component m state
mkComponent3 arg1 arg2 arg3 acquire f = Component
  [ SomeAttribute (mkAttr :: Attribute a) (Const arg1)
  , SomeAttribute (mkAttr :: Attribute b) (Const arg2)
  , SomeAttribute (mkAttr :: Attribute c) (Const arg3)
  ] (\attrs -> withAttrVal attrs arg1 \v1 ->
               withAttrVal attrs arg2 \v2 ->
               withAttrVal attrs arg3 \v3 -> acquire v1 v2 v3)
    $ \attrs state -> withAttrVal attrs arg1 \v1 ->
                      withAttrVal attrs arg2 \v2 ->
                      withAttrVal attrs arg3 \v3 -> f v1 v2 v3 state

mkComponent4 :: forall a b c d m state. (Attr a, Attr b, Attr c, Attr d) => String -> String -> String -> String -> (a -> b -> c -> d -> VulkanResources -> ResourcesStore -> IO ()) -> (a -> b -> c -> d -> Maybe state -> [(Word32, M44 Scalar)] -> m ()) -> Component m state
mkComponent4 arg1 arg2 arg3 arg4 acquire f = Component
  [ SomeAttribute (mkAttr :: Attribute a) (Const arg1)
  , SomeAttribute (mkAttr :: Attribute b) (Const arg2)
  , SomeAttribute (mkAttr :: Attribute c) (Const arg3)
  , SomeAttribute (mkAttr :: Attribute d) (Const arg4)
  ] (\attrs -> withAttrVal attrs arg1 \v1 ->
               withAttrVal attrs arg2 \v2 ->
               withAttrVal attrs arg3 \v3 ->
               withAttrVal attrs arg4 \v4 -> acquire v1 v2 v3 v4)
    $ \attrs state -> withAttrVal attrs arg1 \v1 ->
                      withAttrVal attrs arg2 \v2 ->
                      withAttrVal attrs arg3 \v3 ->
                      withAttrVal attrs arg4 \v4 -> f v1 v2 v3 v4 state

mkComponent5 :: forall a b c d e m state. (Attr a, Attr b, Attr c, Attr d, Attr e) => String -> String -> String -> String -> String -> (a -> b -> c -> d -> e -> VulkanResources -> ResourcesStore -> IO ()) -> (a -> b -> c -> d -> e -> Maybe state -> [(Word32, M44 Scalar)] -> m ()) -> Component m state
mkComponent5 arg1 arg2 arg3 arg4 arg5 acquire f = Component
  [ SomeAttribute (mkAttr :: Attribute a) (Const arg1)
  , SomeAttribute (mkAttr :: Attribute b) (Const arg2)
  , SomeAttribute (mkAttr :: Attribute c) (Const arg3)
  , SomeAttribute (mkAttr :: Attribute d) (Const arg4)
  , SomeAttribute (mkAttr :: Attribute e) (Const arg5)
  ] (\attrs -> withAttrVal attrs arg1 \v1 ->
               withAttrVal attrs arg2 \v2 ->
               withAttrVal attrs arg3 \v3 ->
               withAttrVal attrs arg4 \v4 ->
               withAttrVal attrs arg5 \v5 -> acquire v1 v2 v3 v4 v5)
    $ \attrs state -> withAttrVal attrs arg1 \v1 ->
                      withAttrVal attrs arg2 \v2 ->
                      withAttrVal attrs arg3 \v3 ->
                      withAttrVal attrs arg4 \v4 ->
                      withAttrVal attrs arg5 \v5 -> f v1 v2 v3 v4 v5 state

mkComponent6 :: forall a b c d e f m state. (Attr a, Attr b, Attr c, Attr d, Attr e, Attr f) => String -> String -> String -> String -> String -> String -> (a -> b -> c -> d -> e -> f -> VulkanResources -> ResourcesStore -> IO ()) -> (a -> b -> c -> d -> e -> f -> Maybe state -> [(Word32, M44 Scalar)] -> m ()) -> Component m state
mkComponent6 arg1 arg2 arg3 arg4 arg5 arg6 acquire f = Component
  [ SomeAttribute (mkAttr :: Attribute a) (Const arg1)
  , SomeAttribute (mkAttr :: Attribute b) (Const arg2)
  , SomeAttribute (mkAttr :: Attribute c) (Const arg3)
  , SomeAttribute (mkAttr :: Attribute d) (Const arg4)
  , SomeAttribute (mkAttr :: Attribute e) (Const arg5)
  , SomeAttribute (mkAttr :: Attribute f) (Const arg6)
  ] (\attrs -> withAttrVal attrs arg1 \v1 ->
               withAttrVal attrs arg2 \v2 ->
               withAttrVal attrs arg3 \v3 ->
               withAttrVal attrs arg4 \v4 ->
               withAttrVal attrs arg5 \v5 ->
               withAttrVal attrs arg6 \v6 -> acquire v1 v2 v3 v4 v5 v6)
    $ \attrs state -> withAttrVal attrs arg1 \v1 ->
                      withAttrVal attrs arg2 \v2 ->
                      withAttrVal attrs arg3 \v3 ->
                      withAttrVal attrs arg4 \v4 ->
                      withAttrVal attrs arg5 \v5 ->
                      withAttrVal attrs arg6 \v6 -> f v1 v2 v3 v4 v5 v6 state

mkComponent7 :: forall a b c d e f g m state. (Attr a, Attr b, Attr c, Attr d, Attr e, Attr f, Attr g) => String -> String -> String -> String -> String -> String -> String -> (a -> b -> c -> d -> e -> f -> g -> VulkanResources -> ResourcesStore -> IO ()) -> (a -> b -> c -> d -> e -> f -> g -> Maybe state -> [(Word32, M44 Scalar)] -> m ()) -> Component m state
mkComponent7 arg1 arg2 arg3 arg4 arg5 arg6 arg7 acquire f = Component
  [ SomeAttribute (mkAttr :: Attribute a) (Const arg1)
  , SomeAttribute (mkAttr :: Attribute b) (Const arg2)
  , SomeAttribute (mkAttr :: Attribute c) (Const arg3)
  , SomeAttribute (mkAttr :: Attribute d) (Const arg4)
  , SomeAttribute (mkAttr :: Attribute e) (Const arg5)
  , SomeAttribute (mkAttr :: Attribute f) (Const arg6)
  , SomeAttribute (mkAttr :: Attribute g) (Const arg7)
  ] (\attrs -> withAttrVal attrs arg1 \v1 ->
               withAttrVal attrs arg2 \v2 ->
               withAttrVal attrs arg3 \v3 ->
               withAttrVal attrs arg4 \v4 ->
               withAttrVal attrs arg5 \v5 ->
               withAttrVal attrs arg6 \v6 ->
               withAttrVal attrs arg7 \v7 -> acquire v1 v2 v3 v4 v5 v6 v7)
    $ \attrs state -> withAttrVal attrs arg1 \v1 ->
                      withAttrVal attrs arg2 \v2 ->
                      withAttrVal attrs arg3 \v3 ->
                      withAttrVal attrs arg4 \v4 ->
                      withAttrVal attrs arg5 \v5 ->
                      withAttrVal attrs arg6 \v6 ->
                      withAttrVal attrs arg7 \v7 -> f v1 v2 v3 v4 v5 v6 v7 state

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

-- Convert to the storage representation of an attribute
toAttrRefType :: forall a. Attr a => a -> AttrRef a
toAttrRefType a = case mkAttr :: Attribute a of
  StringAttribute -> pack a
  V3Attribute     -> v3ToTriple a
  V2Attribute     -> v2ToTuple a
  IntAttribute    -> a
  FloatAttribute  -> a
  BoolAttribute   -> a
  ColorAttribute  -> v4ToImVec4 a
  Mat3Attribute   -> 0 -- TODO
  Mat4Attribute   -> 0 -- TODO

-- Convert from the storage representation of an attribute
fromAttrRefType :: forall a. Attr a => AttrRef a -> a
fromAttrRefType a = case mkAttr :: Attribute a of
  StringAttribute -> unpack a
  V3Attribute     -> tripleToV3 a
  V2Attribute     -> tupleToV2 a
  IntAttribute    -> a
  FloatAttribute  -> a
  BoolAttribute   -> a
  ColorAttribute  -> imVec4ToV4 a
  Mat3Attribute   -> identity -- TODO
  Mat4Attribute   -> identity -- TODO

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

-- Uses 'Generic' instance to define fields
mkComponent
  :: forall comp m state.
     HasRecordAttributes comp
  => (comp -> VulkanResources -> ResourcesStore -> IO ())
  -> (comp -> Maybe state -> [(Word32, M44 Scalar)] -> m ())
  -> Component m state
mkComponent acquireF drawF =
  Component
    { attributes = toAttributeList @comp
    , acquire    = \hm vres store ->
        acquireF (fromHashMap hm) vres store
    , draw       = \hm mState instances ->
        drawF (fromHashMap hm) mState instances
    }

{- Generic GLSL struct definitions -}

class GlslType a where
  glslTypeName :: Proxy a -> String

instance GlslType Float where
  glslTypeName _ = "float"

instance GlslType (V2 Float) where
  glslTypeName _ = "vec2"

instance GlslType (V3 Float) where
  glslTypeName _ = "vec3"

instance GlslType (V4 Float) where
  glslTypeName _ = "vec4"

instance GlslType (M33 Float) where
  glslTypeName _ = "mat3"

instance GlslType (M44 Float) where
  glslTypeName _ = "mat4"

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
