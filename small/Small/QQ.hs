{-# LANGUAGE QuasiQuotes #-}

module Small.QQ where

import Data.String.QM (qm)

frag :: String
frag = [qm|
#version 450

layout(location = 0) out vec4 outColor;

void main() {
  outColor = vec4(1, 1, 1, 1);
}
|]
