{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Hickory.FRP.Editor.Types where

import qualified Reactive.Banana as B
import Hickory.Types (Size (..))
import Linear (M44, (^/), translation, V3, V4)
import DearImGui (ImVec4 (..))
import Data.IORef (IORef)
import GHC.Generics (Generic)
import Control.Lens (traversed, toListOf)
import Hickory.Math (Scalar, Mat44)
import Data.Text (Text)
import Data.Generics.Labels ()
import Data.HashMap.Strict (HashMap)
import Hickory.Vulkan.Forward.Types (CommandT, CommandMonad)
import Text.Read.Lex (Lexeme(..))
import qualified Text.Read.Lex as Lex
import GHC.Read (Read (..))
import Text.ParserCombinators.ReadPrec (lift)
import Text.ParserCombinators.ReadP (readS_to_P, between, string, skipSpaces)
import Hickory.Graphics (MatrixT(..), MatrixMonad)
import Hickory.Resources (Resources)
import Control.Monad.Reader (ReaderT, MonadReader)
import Data.Functor.Identity (Identity (..))
import Data.Functor.Const (Const)
import Type.Reflection (TypeRep, typeRep, eqTypeRep, type (:~~:) (..))
import qualified Data.HashMap.Strict as Map

data CameraMoveMode = Pan | Rotate | Zoom
  deriving Eq

data CameraViewMode = OrthoTop | OrthoFront | PerspView
  deriving Eq

data ObjectManipMode = OTranslate | OScale | ORotate
  deriving Eq

data CameraState = CameraState
  { viewMat  :: Mat44
  , projMat  :: Mat44
  , viewMode :: CameraViewMode
  , focusPos :: V3 Scalar
  , angleVec :: V3 Scalar
  , up       :: V3 Scalar
  } deriving Generic

data Object = Object
  { transform   :: M44 Scalar
  , color       :: V4 Scalar
  , model       :: String
  , texture     :: String
  , lit         :: Bool
  , castsShadow :: Bool
  , blend       :: Bool
  , specularity :: Scalar
  , components  :: HashMap String (HashMap String (SomeAttribute Identity))
  } deriving (Generic, Show, Read)

data Component = Component
  { attributes :: [SomeAttribute (Const String)]
  , draw       :: forall m. (MatrixMonad m, MonadReader Resources m, CommandMonad m) => HashMap String (SomeAttribute Identity) -> m ()
  }

class Attr a where
  mkAttr :: Attribute a

instance Attr String where mkAttr = StringAttribute
instance Attr Float  where mkAttr = FloatAttribute
instance Attr Int    where mkAttr = IntAttribute
instance Attr Bool   where mkAttr = BoolAttribute

withAttrVal :: forall a b. Attr a => HashMap String (SomeAttribute Identity) -> String -> (a -> b) -> b
withAttrVal attrs name f = case Map.lookup name attrs of
  Just (SomeAttribute attr (Identity v)) -> case eqAttr attr (mkAttr :: Attribute a) of
    Just HRefl -> f v
    Nothing -> error "Wrong type for attribute"
  Nothing -> f $ defaultAttrVal (mkAttr :: Attribute a)

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
  AttrClasses :: Eq a => AttrClasses a

-- Prove that each attribute has some necessary instances
proveAttrClasses :: Attribute a -> AttrClasses a
proveAttrClasses = \case
  StringAttribute -> AttrClasses
  FloatAttribute  -> AttrClasses
  IntAttribute    -> AttrClasses
  BoolAttribute   -> AttrClasses

eqAttr :: Attribute a1 -> Attribute a2 -> Maybe (a1 :~~: a2)
eqAttr a b = eqTypeRep (typeOfAttr a) (typeOfAttr b)

data SomeAttribute contents = forall a. SomeAttribute { attr :: Attribute a, contents :: contents a }

instance Show (SomeAttribute Identity) where
  show (SomeAttribute attr val) = "SomeAttribute " ++ case attr of
    StringAttribute -> "StringAttribute (" ++ show val ++ ""
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

defaultAttrVal :: Attribute a -> a
defaultAttrVal = \case
  StringAttribute -> ""
  FloatAttribute -> 0
  IntAttribute -> 0
  BoolAttribute -> False

data EditorState = EditorState
  { posRef         :: IORef (Float, Float, Float)
  , rotRef         :: IORef (Float, Float, Float)
  , scaRef         :: IORef (Float, Float, Float)
  , colorRef       :: IORef ImVec4
  , modelRef       :: IORef Text
  , textureRef     :: IORef Text
  , litRef         :: IORef Bool
  , castsShadowRef :: IORef Bool
  , blendRef       :: IORef Bool
  , specularityRef :: IORef Scalar
  , componentsRef  :: IORef [String]
  , componentData  :: HashMap (String,String) (SomeAttribute IORef)
  }

data EditorChange a = EditorChange
  { ev     :: B.Event a
  , setVal :: a -> IO ()
  }

bimapEditorChange :: (B.Event a -> B.Event b) -> ((a -> IO ()) -> b -> IO ())
  -> EditorChange a
  -> EditorChange b
bimapEditorChange f g EditorChange {..} =
  EditorChange { ev = f ev, setVal = g setVal }

data EditorChangeEvents = EditorChangeEvents
  { posChange         :: EditorChange (V3 Scalar)
  , scaChange         :: EditorChange (V3 Scalar)
  , rotChange         :: EditorChange (V3 Scalar)
  , colorChange       :: EditorChange (V4 Scalar)
  , modelChange       :: EditorChange String
  , textureChange     :: EditorChange String
  , litChange         :: EditorChange Bool
  , castsShadowChange :: EditorChange Bool
  , blendChange       :: EditorChange Bool
  , specularityChange :: EditorChange Scalar
  , componentsChange  :: EditorChange [String]
  , componentChanges  :: HashMap (String, String) (SomeAttribute EditorChange)
  }

avg :: [V3 Scalar] -> V3 Scalar
avg vs = sum vs ^/ (fromIntegral $ length vs)

avgObjTranslation :: Traversable t => t Object -> V3 Scalar
avgObjTranslation objs = avg $ toListOf (traversed . #transform . translation) objs
