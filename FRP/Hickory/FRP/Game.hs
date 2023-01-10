{-# LANGUAGE RecursiveDo #-}

module Hickory.FRP.Game where

import Hickory.FRP.Combinators (unionFirst)
import Data.Time (NominalDiffTime)
import qualified Reactive.Banana as B
import qualified Reactive.Banana.Frameworks as B
import Hickory.Math (Scalar, Interpolatable (glerp))
import Hickory.Vulkan.Forward.Types (Renderer, CommandMonad, RenderSettings)
import Hickory.FRP.CoreEvents (CoreEvents (..))
import Hickory.Vulkan.Types (FrameContext)
import Linear (V2(..))
import Hickory.Types (Size(..))
import Control.Lens (view, _1)
import Reactive.Banana ((<@>))
import Data.Maybe (maybeToList)
import Data.List (nub)
import Data.Bool (bool)
import Hickory.Input (Key(..))
import Hickory.Vulkan.Forward.Renderer (pickObjectID)
import Control.Monad.IO.Class (liftIO)
import Hickory.FRP.Historical (historicalWithEvents)
import Hickory.Graphics (MatrixMonad)
import Hickory.Resources (Resources)
import Control.Monad.Reader.Class (MonadReader)
import Hickory.Camera (Camera)
import GHC.Generics (Generic)

-- Queue up events and release in a batch
-- For example, to collect a frame's worth of input events and process at
-- the start of the next frame.
batchEvents
  :: B.Event [acc]
  -> B.Event a
  -> B.MomentIO (B.Event (a, [acc]))
batchEvents accEv batchEv = do
  (ev, _be) <- B.mapAccum [] $ B.mergeWith
    (\acc accs -> (Nothing, acc ++ accs))
    (\a acc -> (Just (a, acc), []))
    (\acc a acc2 -> (Just (a, acc ++ acc2), []))
    accEv
    batchEv

  pure $ B.filterJust ev

chunkTime :: NominalDiffTime -> B.Event NominalDiffTime -> B.MomentIO (B.Event NominalDiffTime, B.Behavior Scalar)
chunkTime chunkSize e = do
  (ev,b) <- B.mapAccum 0 $ (\chunk acc -> if chunk + acc > chunkSize
                                           then (Just chunkSize, acc + chunk - chunkSize)
                                           else (Nothing, chunk + acc))
                         <$> e
  pure (B.filterJust ev, realToFrac . (/chunkSize) <$> b)

timeStep :: NominalDiffTime -> B.Event [input] -> B.Event NominalDiffTime -> B.MomentIO (B.Behavior Scalar, B.Event (NominalDiffTime, [input]))
timeStep physicsTimeStep controlComs eInGameTime = do
  (   eGameTime -- ticks with a new chunk of game time whenever enough has been accumulated
    , frameFraction -- the fraction of a chunk we have progressed
    ) <- chunkTime physicsTimeStep eInGameTime
  eFrameInput <- batchEvents controlComs eGameTime
  pure (frameFraction, eFrameInput)

loadOrStep :: B.Event state -> B.Event (state -> (state, [event])) -> B.Event (state -> (state, [event]))
loadOrStep eLoad eStep = unionFirst
  [ eStep
  , const <$> ((,[]) <$> eLoad)
  ]

type Step input gameState gameEvent = (NominalDiffTime, [input]) -> gameState -> (gameState, [gameEvent])

-- This game loop features
-- - Pausing
-- - Rewind/Forward
-- - Interpolating game state (for less frequent logic updates)
-- - Selecting objects in pause mode (for debugging)
-- TODO: Ideally, screen picking is supported through an iface,
-- rather than using a specific renderer
gameNetwork
  :: forall gameState input gameEvent
   . Interpolatable gameState
  => NominalDiffTime
  -> Key
  -> CoreEvents (Renderer, FrameContext)
  -> gameState
  -> B.Event gameState
  -> B.Event [input]
  -> B.Behavior (Step input gameState gameEvent)
  -> B.MomentIO (B.Behavior gameState, B.Event [gameEvent], B.Behavior Bool)
gameNetwork logicTimeStep pauseKey coreEvents initialState eLoadState eInput step = mdo
  let frameSelect = B.whenE paused $
        unionFirst [ 1 <$ keyDownOrHeld coreEvents Key'Left
                   , 5 <$ keyDownOrHeld coreEvents Key'LeftBracket
                   , (-1) <$ keyDownOrHeld coreEvents Key'Right
                   , (-5) <$ keyDownOrHeld coreEvents Key'RightBracket
                   ]

  paused <- B.accumB False $ not <$ B.filterE (== pauseKey) (keyDown coreEvents)

  replayQueue :: B.Behavior [gameState] <- B.accumB [] $ B.unions
    [ tail <$ B.whenE bShowingReplay eInput
    -- fmap const . buildDebugReplay <$> params <*> mdl <@ B.filterE (== Key'J) (keyDown coreEvents)
    ]

  let bShowingReplay = not . null <$> replayQueue

  (frameFraction, eFrameInput) <- do
    timeStep logicTimeStep eInput $
      unionFirst [ B.whenE bShowingReplay $ (/4) <$> eTime coreEvents
                 , B.whenE (not <$> bShowingReplay) $ B.whenE (not <$> paused) (eTime coreEvents)
                 ]

  (currentGDPair, gameEvs) <- historicalWithEvents
    initialState
    (loadOrStep eLoadState (step <@> B.whenE (not <$> bShowingReplay) eFrameInput))
    frameSelect

  let replayGDPair = (\case
        x1:x2:_ -> (x1, x2)
        x:_ -> (x,x)
        [] -> (error "No replay data", error "Er")
        ) <$> replayQueue
      gdPair = bool <$> currentGDPair <*> replayGDPair <*> bShowingReplay

      gd  = uncurry . glerp <$> frameFraction <*> gdPair


  pure (gd, gameEvs, paused)

accumSelectedObjIds :: CoreEvents (Renderer, FrameContext) -> B.MomentIO (B.Behavior [Int])
accumSelectedObjIds coreEvents = do
  let eClick = (\(Size w h) (_, V2 x y, _) -> (x/ realToFrac w, y/ realToFrac h))
           <$> scrSizeB coreEvents
           <@> fmap head (B.filterE ((<0.3) . view _1 . head) $ eTouchesUp coreEvents)
  renInfo <- B.stepper undefined (eRender coreEvents)
  eScreenPickedObjectID <- fmap (\x -> if x > 0 then Just x else Nothing) <$> B.execute (((\(r,fc) -> liftIO . pickObjectID fc r) <$> renInfo) <@> eClick)
  B.accumB [] $ unionFirst
    [ maybe id (\x -> nub . (x:)) <$> B.whenE (keyHeldB coreEvents Key'LeftShift) eScreenPickedObjectID
    , const . maybeToList <$> eScreenPickedObjectID
    ]

data Scene m = Scene
  { render3DView   :: m ()
  , renderOverlay  :: m ()
  , camera         :: Camera
  , selectedIds    :: [Int]
  , renderSettings :: RenderSettings -> RenderSettings
  } deriving Generic
