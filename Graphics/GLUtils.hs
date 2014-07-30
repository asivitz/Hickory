module Graphics.GLUtils where

import Foreign.C.Types

newtype TexID = TexID CInt deriving (Eq, Ord, Show)

nullTex :: TexID
nullTex = TexID (-1)
