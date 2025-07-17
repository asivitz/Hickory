{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Small.Foo where

import Data.String.QM (qt)
import Data.Text (Text)
import Small.QQ (frag)

import Vulkan.Utils.ShaderQQ.GLSL.Glslang (compileShaderQ)

foo :: Int
foo = 1

bar :: Text
bar = [qt|
bartext
|]

compiled = $(compileShaderQ Nothing "frag" Nothing frag)
