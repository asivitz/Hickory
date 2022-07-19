module Hickory.Math.VectorMatrix where

import Hickory.Math.Matrix
import Hickory.Math.Vector
import Linear (V2(..), V3(..), V4(..), lerp, (^/), _xyz, _w, _z, (!*), inv44, (^*))

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Utils
import Control.Lens

v3rotate :: V3 Scalar -> V3 Scalar -> Scalar -> V3 Scalar
v3rotate v axis ang = (mkRotation axis ang !* v3tov4 v 1) ^. _xyz

m44overV3 :: Mat44 -> V3 Scalar -> V3 Scalar
m44overV3 m v = (m !* v3tov4 v 1) ^. _xyz

withVec4 :: V4 Scalar -> (Ptr CFloat -> IO b) -> IO b
withVec4 v f = with (fmap realToFrac v :: (V4 CFloat)) (f . castPtr)

viewProject :: Mat44 -> V4 Scalar -> V3 Scalar -> V3 Scalar
viewProject modelView (V4 vpx vpy vpw vph) pos =
  let homogenousClipSpace  = modelView !* v3tov4 pos 1
      -- Bring into normalized device space
      (V3 ndx ndy ndz) = (homogenousClipSpace ^. _xyz) ^/ (homogenousClipSpace ^. _w)
  in V3 (vpx + (ndx + 1) / 2 * vpw)
        (vpy + (ndy + 1) / 2 * vph)
        ndz

viewUnproject :: V3 Scalar -> Mat44 -> V4 Scalar -> V3 Scalar
viewUnproject (V3 wx wy wz) modelView (V4 vpx vpy vpw vph) =
  V3 homViewX homViewY homViewZ ^* (1/homViewW) -- un-homogenize
  where
  normalizedDeviceSpace = V3 (2 * (wx - vpx) / vpw - 1) (2 * (wy - vpy) / vph - 1) wz
  -- Bring into homogenous view coords
  (V4 homViewX homViewY homViewZ homViewW) = inv44 modelView !* v3tov4 normalizedDeviceSpace 1

-- This performs an unprojection on the near plane and another on the far
-- plane, and then interpolates between them at the specified depth.
-- Use this to pick a specific point in 3D space from a screen position.
lerpUnproject :: V2 Scalar -> Scalar -> Mat44 -> V4 Scalar -> V3 Scalar
lerpUnproject (V2 x y) depth modelView vp =
        let winnear = V3 x y 0
            winfar = V3 x y 1
            upnear = viewUnproject winnear modelView vp
            upfar = viewUnproject winfar modelView vp
            u = (depth - (upnear ^. _z)) / ((upfar ^. _z) - (upnear ^. _z)) in
                lerp u upfar upnear
