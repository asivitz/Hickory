{-# LANGUAGE RecursiveDo, OverloadedRecordDot #-}

module Hickory.FRP.Game where

import Hickory.FRP.Combinators (unionFirst)
import Data.Time (NominalDiffTime)
import qualified Reactive.Banana as B
import qualified Reactive.Banana.Frameworks as B
import Hickory.Math (Scalar, Interpolatable (glerp))
import Hickory.Vulkan.Renderer.Types (Renderer, RenderSettings)
import Hickory.FRP.CoreEvents (CoreEvents (..))
import Hickory.Vulkan.Types (FrameContext)
import Linear (V2(..), nearZero)
import Hickory.Types (Size(..))
import Reactive.Banana ((<@>), (<@))
import Data.Maybe (maybeToList)
import Data.List (nub)
import Data.Bool (bool)
import Hickory.Input (Key(..), PointUp(..))
import Hickory.Vulkan.Renderer.Renderer (pickObjectID)
import Control.Monad.IO.Class (liftIO)
import Hickory.FRP.Historical (historicalWithEvents)
import Hickory.Camera (Camera)
import GHC.Generics (Generic)
import qualified Data.Sequence as S
import Data.Fixed (mod')

-- Queue up events and release in a batch
-- For example, to collect a frame's worth of input events and process at
-- the start of the next frame.
-- Inputs are combined with `<>`, with the newer input as the left argument
batchEvents
  :: Monoid acc
  => B.Event acc
  -> B.Event a
  -> B.MomentIO (B.Event (a, acc))
batchEvents accEv batchEv = do
  (ev, _be) <- B.mapAccum mempty $ B.mergeWith
    (\acc accs -> (Nothing, acc <> accs))
    (\a acc -> (Just (a, acc), mempty))
    (\acc a acc2 -> (Just (a, acc <> acc2), mempty))
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

type Step input gameState gameEvent = (NominalDiffTime, [input]) -> gameState -> (gameState, [gameEvent])

-- This game loop features
-- - Pausing
-- - Rewind/Forward, slow motion replay
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
  -> B.Event [gameState]
  -> B.MomentIO (B.Behavior gameState, B.Event [gameEvent], B.Behavior Bool)
gameNetwork logicTimeStep pauseKey coreEvents initialState eLoadState eInput step eReplay = mdo
  let frameSelect = B.whenE paused . fmap ((realToFrac . round) .) $
        unionFirst [ (+1) <$ keyDownOrHeld coreEvents Key'Left
                   , (+5) <$ keyDownOrHeld coreEvents Key'LeftBracket
                   , subtract 1 <$ keyDownOrHeld coreEvents Key'Right
                   , subtract 5 <$ keyDownOrHeld coreEvents Key'RightBracket
                   , const 0 <$ eFlipPause
                   , const 0 <$ eLoadState
                   ]

  bAuto <- B.accumB False $ B.unions
    [ not <$ B.filterE (== Key'S) (keyDown coreEvents)
    , const False <$ eFlipPause
    ]

  bSpeedFactor <- B.stepper 1 . B.whenE paused $ unionFirst
    [ 1 <$ B.filterE (== Key'0) (keyDown coreEvents)
    , 0.01 <$ B.filterE (== Key'1) (keyDown coreEvents)
    , 0.03 <$ B.filterE (== Key'2) (keyDown coreEvents)
    , 0.07 <$ B.filterE (== Key'3) (keyDown coreEvents)
    , 0.1 <$ B.filterE (== Key'4) (keyDown coreEvents)
    , 0.2 <$ B.filterE (== Key'5) (keyDown coreEvents)
    , 0.3 <$ B.filterE (== Key'6) (keyDown coreEvents)
    , 0.4 <$ B.filterE (== Key'7) (keyDown coreEvents)
    , 0.5 <$ B.filterE (== Key'8) (keyDown coreEvents)
    , 0.7 <$ B.filterE (== Key'9) (keyDown coreEvents)
    ]

  bCurrentFrameSelect <- B.accumB 0 $ B.unions
    [ frameSelect
    , B.whenE bAuto $ (\factor time acc -> max 0 (acc - realToFrac time * factor / realToFrac logicTimeStep)) <$> bSpeedFactor <@> eTime coreEvents
    ]

  let eFlipPause = B.filterE (== pauseKey) (keyDown coreEvents)
      eUnpause = B.whenE paused eFlipPause

  paused <- B.accumB False $ not <$ eFlipPause

  replayQueue :: B.Behavior [gameState] <- B.accumB [] $ B.unions
    [ tail <$ B.whenE bShowingReplay eFrameInput
    , const <$> eReplay
    ]

  let bShowingReplay = not . null <$> replayQueue

  (frameFraction, eFrameInput) <- do
    timeStep logicTimeStep eInput $
      unionFirst [ B.whenE bShowingReplay $ (/4) <$> eTime coreEvents
                 , B.whenE (not <$> bShowingReplay) $ B.whenE (not <$> paused) (eTime coreEvents)
                 ]

  (history, gameEvs) <- historicalWithEvents
    initialState
    (B.unionWith const (const <$> ((,[]) <$> eLoadState)) (step <@> B.whenE (not <$> bShowingReplay) eFrameInput))
    ((\f -> f <$> bCurrentFrameSelect <@ eUnpause) (S.drop . floor))

  let bCurrentFrameFraction = (\f -> f <$> frameFraction <*> bCurrentFrameSelect <*> paused) \frac frame p ->
        if p then 1 - (frame `mod'` 1) else frac
      actualGDPair = (\f -> f <$> history <*> bCurrentFrameSelect) \s (floor -> i) ->
        let from = S.index s (max 0 (min (i + 1) (S.length s - 1)))
            to = S.index s (max 0 (min (S.length s - 1) i))
        in (from, to)

      replayGDPair = (\case
        x1:x2:_ -> (x1, x2)
        x:_ -> (x,x)
        [] -> (error "No replay data", error "Er")
        ) <$> replayQueue
      gdPair = bool <$> actualGDPair <*> replayGDPair <*> bShowingReplay
      gd = (\f -> f <$> bCurrentFrameFraction <*> gdPair) \ff pair -> if nearZero ff then fst pair else uncurry (glerp ff) pair

  pure (gd, gameEvs, paused)

data Scene m = Scene
  { render3DView   :: m ()
  , renderOverlay  :: m ()
  , camera         :: Camera
  , selectedIds    :: [Int]
  , renderSettings :: RenderSettings -> RenderSettings
  } deriving Generic
