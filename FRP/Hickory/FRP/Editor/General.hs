module Hickory.FRP.Editor.General where

import qualified Reactive.Banana as B
import Hickory.FRP.CoreEvents (CoreEvents (..))
import qualified Reactive.Banana.Frameworks as B
import Hickory.Math (rlerp, v3tov4, Scalar)
import Hickory.Types (Size (..))
import Hickory.FRP.Combinators (unionFirst)
import Linear (M44, column, (!*), V3 (..), V2 (..), V4 (..), norm, _x, _y, _z)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, readIORef, writeIORef)
import Hickory.FRP.Editor.Types (EditorChange(..))
import Control.Lens ((^.))

-- Move this into better general use modules

refChangeEvent :: (Eq a) => CoreEvents b -> IORef a -> B.MomentIO (EditorChange a)
refChangeEvent coreEvents ref = do
  val <- liftIO $ readIORef ref

  (ePushedVal, pushVal) <- B.newEvent -- Change the val without generating a change event

  e <- B.execute $ liftIO (readIORef ref) <$ eRender coreEvents
  (ev, _) <- B.mapAccum val $ unionFirst
    [ (\newVal acc -> (if newVal == acc then Nothing else Just newVal,newVal)) <$> e
    , (\newVal _acc -> (Nothing, newVal)) <$> ePushedVal
    ]
  pure $ EditorChange (B.filterJust ev) (\x -> pushVal x >> writeIORef ref x)

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
