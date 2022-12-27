{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE GADTs #-}

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
import Hickory.Vulkan.Forward.Types (DrawCommand, Command, CommandT)
import Text.Read.Lex (Lexeme(..))
import qualified Text.Read.Lex as Lex
import GHC.Read (Read (..))
import Text.ParserCombinators.ReadPrec (lift)
import Text.ParserCombinators.ReadP (readS_to_P, between, string, skipSpaces)
import Hickory.Graphics (MatrixT(..))
import Hickory.Resources (Resources)
import Control.Monad.Reader (ReaderT)
import Data.Functor.Identity (Identity)
import Data.Functor.Const (Const)
import Data.Bifunctor (Bifunctor)

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
  , focusPlaneSize :: Size Scalar
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
  { name       :: String
  , attributes :: [SomeAttribute (Const String)]
  , draw       :: [SomeAttribute Identity] -> MatrixT (ReaderT Resources (CommandT Identity)) ()
  } deriving Generic

data Attribute a contents where
  StringAttribute :: contents -> Attribute String contents
  FloatAttribute  :: contents -> Attribute Float contents
  IntAttribute    :: contents -> Attribute Int contents
  BoolAttribute   :: contents -> Attribute Bool contents

data SomeAttribute container = forall a. SomeAttribute { unSomeAttribute :: Attribute a (container a) }

instance Show (SomeAttribute Identity) where
  show (SomeAttribute x) = "SomeAttribute (" ++ case x of
    StringAttribute contents -> "StringAttribute (" ++ show contents ++ "))"
    FloatAttribute contents  -> "FloatAttribute ("  ++ show contents ++ "))"
    IntAttribute contents    -> "IntAttribute ("    ++ show contents ++ "))"
    BoolAttribute contents   -> "BoolAttribute ("   ++ show contents ++ "))"

instance Read (SomeAttribute Identity) where
  readPrec = lift do
    Lex.expect (Ident "SomeAttribute")
    let pars = between (skipSpaces >> string "(") (skipSpaces >> string ")")
    pars do
      Lex.lex >>= \case
        Ident "StringAttribute" -> SomeAttribute . StringAttribute <$> pars (readS_to_P (reads @(Identity String)))
        Ident "FloatAttribute"  -> SomeAttribute . FloatAttribute  <$> pars (readS_to_P (reads @(Identity Float)))
        Ident "IntAttribute"    -> SomeAttribute . IntAttribute    <$> pars (readS_to_P (reads @(Identity Int)))
        Ident "BoolAttribute"   -> SomeAttribute . BoolAttribute   <$> pars (readS_to_P (reads @(Identity Bool)))
        _ -> fail "Invalid attribute type"

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
