module Hickory.Graphics.Types where

import Hickory.Graphics.VAO (VAOObj, VAOConfig)
import qualified Data.Vector.Storable as V
import Graphics.GL.Compatibility41 as GL
import Hickory.Graphics.GLSupport (DrawType, UniformBinding)
import Hickory.Graphics.Textures (TexID)
import Hickory.Math.Matrix (Mat44)

data DrawSpec
  = VAO VAOObj
  | DynVAO VAOConfig (V.Vector GLfloat, V.Vector GLushort, DrawType)
  deriving (Show)

data RenderTree
  = Primitive [UniformBinding] [TexID] DrawSpec
  | List [RenderTree]
  | XForm Mat44 RenderTree
  | NoRender
