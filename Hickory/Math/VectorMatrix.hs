module Hickory.Math.VectorMatrix where

import Hickory.Math.Matrix
import Hickory.Math.Vector
import Linear (V2(..), V3(..), V4(..), lerp, (^/), _xyz, _w, _z, (!*), inv44)

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Utils
import Control.Lens

v3rotate :: V3 Scalar -> V3 Scalar -> Scalar -> V3 Scalar
v3rotate v axis ang = (mkRotation axis ang !* v3tov4 v 1) ^. _xyz

m44overV3 m v = (m !* v3tov4 v 1) ^. _xyz

withVec4 :: V4 Scalar -> (Ptr CFloat -> IO b) -> IO b
withVec4 v f = with (fmap realToFrac v :: (V4 CFloat)) (f . castPtr)

viewProject :: Mat44 -> V4 Scalar -> V3 Scalar -> V3 Scalar
viewProject modelView (V4 vpx vpy vpw vph) pos =
  let projected  = modelView !* v3tov4 pos 1
      (V3 x y z) = fmap (\a -> (a + 1) / 2) $ (projected ^. _xyz) ^/ (projected ^. _w)
  in  V3 (vpx + vpw * x) (vpy + vph * y) z

viewUnproject :: V3 Scalar -> Mat44 -> V4 Scalar -> V3 Scalar
viewUnproject (V3 wx wy wz) modelView (V4 vpx vpy vpw vph) =
        let inverted = inv44 modelView
            inviewport = V3 ((wx - vpx) / vpw) ((wy - vpy) / vph) wz
            adjusted = fmap (\x -> x * 2 - 1) inviewport
            fromv = v3tov4 adjusted 1
            (V4 rx ry rz rw) = inverted !* fromv
            in V3 (rx / rw) (ry / rw) (rz / rw)

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
