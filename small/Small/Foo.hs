{-# LANGUAGE TemplateHaskell #-}

module Small.Foo where

import Small.QQ (frag)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (compileShaderQ)

compiled = $(compileShaderQ Nothing "frag" Nothing frag)
