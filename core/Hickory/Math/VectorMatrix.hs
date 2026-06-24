module Hickory.Math.VectorMatrix where

import Hickory.Math.Matrix
import Hickory.Math.Vector
import Linear (V2(..), V3(..), V4(..), lerp, (^/), _xyz, _w, _z, (!*), inv44, (^*), M44, Epsilon)

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Utils
import Control.Lens

v3rotate :: (Floating a, Epsilon a) => V3 a -> V3 a -> a -> V3 a
v3rotate v axis ang = (mkRotation axis ang !* v3tov4 v 1) ^. _xyz

m44overV3 :: Num a => M44 a -> V3 a -> V3 a
m44overV3 m v = (m !* v3tov4 v 1) ^. _xyz

withVec4 :: V4 Float -> (Ptr CFloat -> IO b) -> IO b
withVec4 v f = with (fmap realToFrac v :: (V4 CFloat)) (f . castPtr)

viewProject :: (Num a, Fractional a) => M44 a -> V4 a -> V3 a -> V3 a
viewProject modelView (V4 vpx vpy vpw vph) pos =
  let homogenousClipSpace  = modelView !* v3tov4 pos 1
      -- Bring into normalized device space
      (V3 ndx ndy ndz) = (homogenousClipSpace ^. _xyz) ^/ (homogenousClipSpace ^. _w)
  in V3 (vpx + (ndx + 1) / 2 * vpw)
        (vpy + (ndy + 1) / 2 * vph)
        ndz

viewUnproject :: (Num a, Fractional a) => V3 a -> M44 a -> V4 a -> V3 a
viewUnproject (V3 wx wy wz) modelView (V4 vpx vpy vpw vph) =
  V3 homViewX homViewY homViewZ ^* (1/homViewW) -- un-homogenize
  where
  normalizedDeviceSpace = V3 (2 * (wx - vpx) / vpw - 1) (2 * (wy - vpy) / vph - 1) wz
  -- Bring into homogenous view coords
  (V4 homViewX homViewY homViewZ homViewW) = inv44 modelView !* v3tov4 normalizedDeviceSpace 1

-- This performs an unprojection on the near plane and another on the far
-- plane, and then interpolates between them at the specified depth.
-- Use this to pick a specific point in 3D space from a screen position.
lerpUnproject :: (Num a, Fractional a) => V2 a -> a -> M44 a -> V4 a -> V3 a
lerpUnproject (V2 x y) depth modelView vp =
        let winnear = V3 x y 0
            winfar = V3 x y 1
            upnear = viewUnproject winnear modelView vp
            upfar = viewUnproject winfar modelView vp
            u = (depth - (upnear ^. _z)) / ((upfar ^. _z) - (upnear ^. _z)) in
                lerp u upnear upfar
