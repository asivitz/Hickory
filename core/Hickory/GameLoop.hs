module Hickory.GameLoop where

import Hickory.Math (Interpolatable (..), Scalar)
import Hickory.Input (InputFrame(..))
import Data.IORef (IORef, newIORef, readIORef, modifyIORef', writeIORef, atomicModifyIORef')
import qualified Hickory.Vulkan.Types as H
import Hickory.Types (Size)
import qualified Hickory.Vulkan.Forward.Types as H
import qualified Data.Sequence as S
import GHC.Compact (getCompact, compact)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Control.Monad (forever)
import Data.Time (NominalDiffTime)
import Control.Monad.IO.Class (MonadIO)
import Hickory.Vulkan.Forward.Types (RenderSettings(..), OverlayGlobals (..), RenderFunction)
import Linear (identity, V4 (..))
import qualified Hickory.Vulkan.Forward.Renderer as H
import Control.Concurrent (threadDelay)
import Hickory.Vulkan.Forward.Types (Scene)
import Acquire.Acquire (Acquire)

pureGameScene :: forall model evs. Interpolatable model => model -> (InputFrame -> model -> (model, evs)) -> IO (InputFrame -> IO (model, model))
pureGameScene initialModel stepFunction = do
  stateRef    :: IORef (S.Seq model) <- newIORef $ S.singleton initialModel
  stateIdxRef :: IORef Int <- newIORef 0
  pure \inputFrame -> do
    lastState <- readIORef stateRef <&> fromMaybe (error "No game states available") . S.lookup 0
    newState <- pure . fst $ stepFunction inputFrame lastState
    modifyIORef' stateRef (\s -> S.take 500 $ newState S.<| s)
    pure (lastState, newState)

  -- mdl <- ((,) <$> readIORef stateRef <*> readIORef stateIdxRef) <&> \(gameSeq, idx) ->
  --   let idx' = min idx (S.length gameSeq - 2)
  --   in case (,) <$> S.lookup idx' gameSeq <*> S.lookup (idx'+1) gameSeq of
  --       Just (to, from) -> glerp (realToFrac $ curInputFrame.delta / physicsTimeStep) from to
  --       Nothing -> fromMaybe (error "No game states available") $ S.lookup 0 gameSeq

