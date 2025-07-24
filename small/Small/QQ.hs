module Small.QQ where

import qualified DearImGui.Raw as Raw
import GHC.Ptr (nullPtr)

frag :: String
frag = "#version 450\nlayout(location = 0) out vec4 outColor;\nvoid main() {\noutColor = vec4(1, 1, 1, 1);\n}"

myBegin :: IO Bool
myBegin = Raw.begin nullPtr (Just nullPtr) Nothing

