{- shooter.hs
 - This game has simple character movement with the arrow keys,
 - as well as shooting simple missiles with the SpaceBar.
 -}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Maybe (maybeToList)
import Data.Time.Clock (NominalDiffTime)
import Hickory.Camera
import Hickory.FRP.CoreEvents (mkCoreEvents, CoreEvents(..), CoreEventGenerators)
import Hickory.FRP.Game (timeStep)
import Hickory.FRP.UI (topLeft)
import Hickory.FRP.Historical (historical)
import Hickory.Input
import Hickory.Math (vnull, mkTranslation, mkScale, viewTarget)
import Hickory.Types
import Linear ( V2(..), V3(..), (^*), V4(..), (!*!))
import Linear.Metric
import Platforms.GLFW.FRP (glfwCoreEventGenerators)
import Reactive.Banana ((<@>))
import qualified Graphics.UI.GLFW as GLFW
import qualified Reactive.Banana as B
import qualified Reactive.Banana.Frameworks as B
import Control.Lens (view)
import Acquire.Acquire (Acquire)

import qualified Platforms.GLFW.Vulkan as GLFWV
import qualified Hickory.Vulkan.Types as H
import qualified Hickory.Vulkan.Text as H
import qualified Hickory.Vulkan.Forward.Types as H
import qualified Hickory.Vulkan.Forward.Renderer as H

import Control.Monad
import Vulkan
  ( pattern FILTER_LINEAR
  , SamplerAddressMode(..)
  )

import Hickory.Vulkan.Vulkan
import qualified Hickory.Vulkan.DescriptorSet as H
import Data.Foldable (for_)
import Hickory.Graphics.DrawText (textcommand)
import Hickory.Text (TextCommand(..), XAlign (..))
import Hickory.Math.Vector (Scalar)
import Hickory.Color (white, red)
import Hickory.Vulkan.Forward.Types (WorldGlobals(..), OverlayGlobals(..), DrawCommand (..))
import qualified Hickory.Vulkan.StockMesh as H
import Platforms.GLFW.Vulkan (initGLFWVulkan)

-- ** GAMEPLAY **

type Vec = V2 Scalar

-- Our game data
data Model = Model
  { playerPos       :: Vec
  , playerMoveDir   :: Vec
  , firingDirection :: Vec
  , missiles        :: [(Vec, Vec)]
  }

-- All the possible inputs
data Msg = Fire | AddMove Vec | SubMove Vec

-- By default, our firingDirection is to the right
newGame :: Model
newGame = Model (V2 0 0) (V2 0 0) (V2 1 0) []

stepF :: (NominalDiffTime, [Msg]) -> Model -> Model
stepF (delta, msgs) mdl = foldr stepMsg (physics delta mdl) msgs

-- Change the game model by processing some input
stepMsg :: Msg -> Model -> Model
stepMsg msg model@Model { playerPos, firingDirection, missiles } = case msg of
  Fire -> model { missiles = (playerPos, firingDirection) : missiles }
  AddMove dir -> adjustMoveDir dir model
  SubMove dir -> adjustMoveDir (- dir) model

-- Step the world forward by some small delta
physics :: NominalDiffTime -> Model -> Model
physics (realToFrac -> delta) model@Model { playerPos, playerMoveDir, missiles } = model
  { playerPos = playerPos + (playerMoveDir ^* (delta * playerMovementSpeed))
  , missiles = filter missileInBounds $
      map (\(pos, dir) -> (pos + (dir ^* (delta * missileMovementSpeed)), dir)) missiles
  }

-- Some gameplay constants
playerMovementSpeed :: Scalar
playerMovementSpeed = 100

missileMovementSpeed :: Scalar
missileMovementSpeed = 200

-- Pure utilities
missileInBounds :: (Vec, Vec) -> Bool
missileInBounds (pos, _) = norm pos < 500

adjustMoveDir :: Vec -> Model -> Model
adjustMoveDir dir model@Model { playerMoveDir, firingDirection } =
  model { playerMoveDir = newMoveDir
        , firingDirection = if vnull newMoveDir then firingDirection else newMoveDir
        }
  where newMoveDir = playerMoveDir + dir

-- ** RENDERING **

-- The resources used by our rendering function
data Resources = Resources
  { circleTex           :: H.PointedDescriptorSet
  , square              :: H.BufferedMesh
  , textRenderer        :: H.TextRenderer
  }

-- Load meshes, textures, materials, fonts, etc.
loadResources :: String -> H.VulkanResources -> Acquire Resources
loadResources path vulkanResources = do
  circleTex <- view #descriptorSet <$> H.withTextureDescriptorSet vulkanResources [(path ++ "images/circle.png", FILTER_LINEAR, SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE)]

  square <- H.withSquareMesh vulkanResources

  -- gidolinya.json (font data) and gidolinya.png (font texture) were
  -- generated using https://github.com/Chlumsky/msdf-atlas-gen
  textRenderer <- H.withTextRenderer vulkanResources (path ++ "fonts/gidolinya.json") (path ++ "images/gidolinya.png") 2

  pure $ Resources {..}

-- Our render function
renderGame :: MonadIO m => Resources -> Size Scalar -> Model -> (H.Renderer, H.FrameContext) -> m ()
renderGame Resources {..} scrSize@(Size w _h) Model { playerPos, missiles } (renderer, frameContext)
  = H.renderToRenderer frameContext renderer renderSettings litF overlayF
  where
  renderSettings = H.RenderSettings
    { worldGlobals = H.worldGlobalDefaults
      { viewMat = viewTarget (V3 0 0 (-1)) (V3 0 0 1) (V3 0 (-1) 0)
      , projMat = shotMatrix (Ortho w 1 100 True) (aspectRatio scrSize)
      }
    , overlayGlobals = OverlayGlobals
      { viewMat = viewTarget (V3 0 0 (-1)) (V3 0 0 1) (V3 0 (-1) 0)
      , projMat = shotMatrix (Ortho w 1 100 False) (aspectRatio scrSize)
      }
    , postSettings = H.postDefaults
    , clearColor = V4 0 0 0 1
    , highlightObjs = []
    }
  litF = do
    for_ missiles \(pos, _) ->
      H.addCommand $ DrawCommand
        { modelMat = mkTranslation pos !*! mkScale (V2 5 5)
        , mesh = H.Buffered square
        , color = white
        , drawType = H.Static $ H.StaticMesh circleTex (V2 1 1)
        , lit = False
        , castsShadow = False
        , blend = True
        , ident = Nothing
        , specularity = 1
        }

    H.addCommand $ DrawCommand
      { modelMat = mkTranslation playerPos !*! mkScale (V2 10 10)
      , mesh = H.Buffered square
      , color = red
      , drawType = H.Static $ H.StaticMesh circleTex (V2 1 1)
      , lit = False
      , castsShadow = False
      , blend = False
      , ident = Nothing
      , specularity = 1
      }

  overlayF = do
    H.drawText textRenderer (mkTranslation (topLeft 20 20 scrSize) !*! mkScale (V2 (12/12) (12/12)))
      white white 0 $ textcommand { text = "Arrow keys move, Space shoots", align = AlignLeft }

-- ** INPUT **

-- Translating raw input to game input
isMoveKey :: Key -> Bool
isMoveKey key = key `elem` ([Key'Up, Key'Down, Key'Left, Key'Right] :: [Key])

moveKeyVec :: Key -> Vec
moveKeyVec key = case key of
  Key'Up    -> V2 0 (-1)
  Key'Down  -> V2 0 1
  Key'Left  -> V2 (-1) 0
  Key'Right -> V2 1 0
  _ -> V2 0 0

procKeyDown :: Key -> Maybe Msg
procKeyDown k =
  if isMoveKey k
  then Just $ AddMove (moveKeyVec k)
  else case k of
    Key'Space -> Just Fire
    _ -> Nothing

procKeyUp :: Key -> Maybe Msg
procKeyUp k =
  if isMoveKey k
  then Just $ SubMove (moveKeyVec k)
  else Nothing

physicsTimeStep :: NominalDiffTime
physicsTimeStep = 1/60

-- Build the FRP network
buildNetwork :: Resources -> CoreEventGenerators (H.Renderer, H.FrameContext) -> IO ()
buildNetwork res evGens = do
  B.actuate <=< B.compile $ mdo
    coreEvents <- mkCoreEvents evGens

    -- Player inputs
    let inputs = mconcat [ maybeToList . procKeyDown <$> keyDown coreEvents
                         , maybeToList . procKeyUp   <$> keyUp coreEvents
                         ]

    -- Collect a frame of input
    (_frameFraction, eFrame) <- timeStep physicsTimeStep inputs (eTime coreEvents)

    -- Step the game model forward every frame
    mdlPair <- historical newGame (stepF <$> eFrame) B.never
    let mdl = snd <$> mdlPair

    -- every time we get a 'render' event tick, draw the screen
    B.reactimate
      (renderGame res <$> (fmap realToFrac <$> scrSizeB coreEvents) <*> mdl <@> eRender coreEvents)

main :: IO ()
main = GLFWV.withWindow 750 750 "Demo" \win -> runAcquire do
  vulkanResources <- initGLFWVulkan win
  res <- loadResources "Example/Shooter/assets/" vulkanResources
  -- setup event generators for core input (keys, mouse clicks, and elapsed time, etc.)
  (coreEvProc, evGens) <- liftIO $ glfwCoreEventGenerators win

  -- build and run the FRP network
  liftIO $ buildNetwork res evGens

  liftIO $ GLFWV.runFrames win vulkanResources (H.withRenderer vulkanResources) \renderer frameContext -> do
    coreEvProc (renderer, frameContext)

    focused <- GLFW.getWindowFocused win
    -- don't consume CPU when the window isn't focused
    unless focused (threadDelay 100000)
