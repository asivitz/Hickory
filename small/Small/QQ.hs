{-# LANGUAGE QuasiQuotes #-}

module Small.QQ where

import Data.String.QM (qm)
import GHC.Generics
import Hickory.Editor (gGlslLines)
import Data.Proxy (Proxy(..))

frag :: String
frag = [qm|
#version 450

layout(location = 0) out vec4 outColor;

void main() {
  outColor = vec4(1, 1, 1, 1);
}
|]

data FieldConstants = FieldConstants
  {
  } deriving Generic

fieldUniformsDef :: [String]
fieldUniformsDef = gGlslLines (Proxy :: Proxy (Rep FieldConstants))

