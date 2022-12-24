{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

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
  } deriving (Generic, Show, Read)

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
  }

type EditorChange a = (B.Event a, a -> IO ())

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
  }

avg :: [V3 Scalar] -> V3 Scalar
avg vs = sum vs ^/ (fromIntegral $ length vs)

avgObjTranslation :: Traversable t => t Object -> V3 Scalar
avgObjTranslation objs = avg $ toListOf (traversed . #transform . translation) objs
