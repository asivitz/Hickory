module Hickory.FRP.Editor.General where

import qualified Reactive.Banana as B
import Hickory.FRP.CoreEvents (CoreEvents (..))
import Hickory.Math (Scalar)
import Linear (M44, column, V3 (..), V2 (..), V4 (..), norm, _x, _y, _z)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Hickory.FRP.Editor.Types (EditorChange(..))
import Control.Lens ((^.))

-- Move this into better general use modules

refChangeEvent :: (Eq a) => IORef a -> IO (EditorChange a)
refChangeEvent ref = do
  lastRef <- readIORef ref >>= newIORef
  let check = do
        cur <- readIORef ref
        lst <- readIORef lastRef
        pure $
          if cur /= lst
          then Just cur
          else Nothing
      set x = do
        writeIORef ref x
        writeIORef lastRef x

  pure $ EditorChange check set

matScale :: Floating a => M44 a -> V3 a
matScale m = V3 (norm $ m ^. column _x) (norm $ m ^. column _y) (norm $ m ^. column _z)

matEuler :: RealFloat a => M44 a -> V3 a
matEuler m =
  if sy < 1e-6
  then let
          x = atan2 (-m12) m11
          y = atan2 (-m20) sy
          z = 0
       in V3 x y z
  else let
          x = atan2 m21 m22
          y = atan2 (-m20) sy
          z = atan2 m10 m00
       in V3 x y z
  where
  V4 (V4 m00 _m01 _m02 _m03)
     (V4 m10 m11 m12 _m13)
     (V4 m20 m21 m22 _m23)
     (V4 _m30 _m31 _m32 _m33) = m
  sy = sqrt (m00 * m00 + m10 * m10)

mkCursorLoc :: B.MonadMoment m => CoreEvents a -> m (B.Behavior (V2 Scalar))
mkCursorLoc coreEvents =
  B.accumB (V2 0 0) (const . fst . head <$> B.filterE (not . null) (eTouchesLoc coreEvents))
