{-# LANGUAGE QuasiQuotes #-}

module Small.QQ where

import Data.String.QM (qm)
import GHC.Generics
import SmallDep.Foo (glslStructDef)
-- import Hickory.Editor (glslStructDef)

frag :: String
frag = [qm|
#version 450

layout(location = 0) out vec4 outColor;

void main() {
  outColor = vec4(1, 1, 1, 1);
}
|]

data FieldConstants = FieldConstants
  { member :: Float
  } deriving Generic

fieldUniformsDef :: String
fieldUniformsDef = glslStructDef @FieldConstants

