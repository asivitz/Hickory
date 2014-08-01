module Math.Vector 
    (
    module Data.Vector.V2,
    module Data.Vector.V3,
    module Data.Vector.V4,
    module Data.Vector.Class,
    V2,
    V3,
    V4,
    v2,
    v3,
    v4,
    pZero,
    mat44TranslateV,
    withVec4,
    v2tov3,
    viewProject,
    lerpUnproject
    )
    where


import Data.Vector.V2
import Data.Vector.V3
import Data.Vector.V4
import Data.Vector.Class
import Math.Matrix
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import System.IO.Unsafe

type V2 = Vector2
type V3 = Vector3
type V4 = Vector4

v2 = Vector2
v3 = Vector3
v4 = Vector4

pZero :: BasicVector a => a
pZero = vpromote 0

v2tov3 :: V2 -> Scalar -> V3
v2tov3 (Vector2 x y) z = v3 x y z

v3tov4 :: V3 -> Scalar -> V4
v3tov4 (Vector3 x y z) w = v4 x y z w

v4tov3 :: V4 -> V3
v4tov3 (Vector4 x y z w) = (Vector3 x y z)

mat44TranslateV :: V3 -> Mat44 -> Mat44
mat44TranslateV (Vector3 x y z) m = 
    mat44Translate (realToFrac x) (realToFrac y) (realToFrac z) $ m

withVec4 :: Vector4 -> (Ptr CFloat -> IO b) -> IO b
withVec4 v func = let lst = map realToFrac $ vunpack v in
    withArray lst func

buildVec4 :: (Ptr CFloat -> IO ()) -> IO V4
buildVec4 f = 
   do ptr <- mallocArray 4
      f ptr
      lst <- peekArray 4 ptr
      free ptr
      let res = vpack (map realToFrac lst)
      case res of
          Nothing -> error "Couldn't create vec"
          Just r -> return r

mat44MulVec4 :: Mat44 -> V4 -> V4
mat44MulVec4 mat v = unsafePerformIO $
        withVec4 v $ \vp -> 
            withMat44 mat $ \mp ->
                buildVec4 $ \rp ->
                    c'mat44MulVec4 rp mp vp

foreign import ccall "mat4x4_mul_vec4" c'mat44MulVec4
    :: Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

viewProject :: V3 -> Mat44 -> V4 -> V3
viewProject pos modelView (Vector4 vpx vpy vpw vph) =
        let pos' = v3tov4 pos 1
            projected = v4tov3 $ mat44MulVec4 modelView pos'
            (Vector3 x y z) = vmap (\a -> (a + 1) / 2) projected in
                v3 (vpx + vpw * x) (vpy + vph * y) z

viewUnproject :: V3 -> Mat44 -> V4 -> V3
viewUnproject (Vector3 wx wy wz) modelView (Vector4 vpx vpy vpw vph) =
        let inverted = mat44Invert modelView
            inviewport = v3 ((wx - vpx) / vpw) ((wy - vpy) / vph) wz
            adjusted = vmap (\x -> x * 2 - 1) inviewport
            from = v3tov4 adjusted 1
            (Vector4 rx ry rz rw) = mat44MulVec4 inverted from in
                v3 (rx / rw) (ry / rw) (rz / rw)

-- This performs an unprojection on the near plane and another on the far
-- plane, and then interpolates between them at the specified depth.
-- Use this to pick a specific point in 3D space from a screen position.
lerpUnproject :: V2 -> Scalar -> Mat44 -> V4 -> V3
lerpUnproject (Vector2 x y) depth modelView vp@(Vector4 vpx vpy vpw vph) =
        let winnear = v3 x (vph - y) 0
            winfar = v3 x (vph - y) 1
            upnear = viewUnproject winnear modelView vp
            upfar = viewUnproject winfar modelView vp
            u = (depth - (v3z upnear)) / ((v3z upfar) - (v3z upnear)) in
                vlinear u upnear upfar
