{- shooter.hs
 - This game has simple character movement with the arrow keys,
 - as well as shooting simple missiles with the SpaceBar.
 -}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Time.Clock (NominalDiffTime)
import Hickory.Camera
import Hickory.FRP.CoreEvents (mkCoreEvents, CoreEvents(..), CoreEventGenerators)
import Hickory.FRP.Game (gameNetwork)
import Hickory.FRP.UI (topLeft)
import Hickory.Input
import Hickory.Math (vnull, mkTranslation, mkScale, viewTarget, Interpolatable (..))
import Hickory.Types
import Linear ( V2(..), V3(..), (^*), V4(..), (!*!), zero)
import Linear.Metric
import Platforms.GLFW.FRP (glfwCoreEventGenerators)
import Reactive.Banana ((<@>))
import qualified Graphics.UI.GLFW as GLFW
import qualified Reactive.Banana as B
import qualified Reactive.Banana.Frameworks as B
import Acquire.Acquire (Acquire)

import qualified Platforms.GLFW.Vulkan as GLFWV
import qualified Hickory.Vulkan.Types as H
import qualified Hickory.Vulkan.Forward.Types as H
import qualified Hickory.Vulkan.Forward.Renderer as H

import Control.Monad
import Vulkan
  ( pattern FILTER_LINEAR
  , SamplerAddressMode(..)
  )

import Hickory.Vulkan.Vulkan
import Data.Foldable (for_)
import Hickory.Graphics.DrawText (textcommand)
import Hickory.Text (TextCommand(..), XAlign (..))
import Hickory.Math.Vector (Scalar)
import Hickory.Color (white, red)
import Hickory.Vulkan.Forward.Types (WorldGlobals(..), OverlayGlobals(..), DrawCommand (..))
import Platforms.GLFW.Vulkan (initGLFWVulkan)
import Hickory.FRP.Combinators (unionAll)
import Data.Bool (bool)
import Hickory.Resources (ResourcesStore(..), withResourcesStore, loadResource, getMesh, getTexture, getResourcesStoreResources, Resources, getSomeFont)
import Control.Monad.Reader (ReaderT (..))

-- ** GAMEPLAY **

type Vec = V2 Scalar

-- Our game data
data Model = Model
  { playerPos :: Vec
  , playerDir :: Vec
  , missiles  :: [(Vec, Vec)]
  }

instance Interpolatable Model where
  glerp fr a b = (if fr > 0.5 then b else a)
    { playerPos = glerp fr (playerPos a) (playerPos b)
    }

-- Input messages
data Msg = Fire

-- By default, our firingDirection is to the right
newGame :: Model
newGame = Model (V2 0 0) (V2 0 0) []

-- Move our game state forward in time
stepF :: Vec -> (NominalDiffTime, [Msg]) -> Model -> (Model, [()])
stepF moveDir (delta, msgs) mdl = (,[]) $ foldr stepMsg (physics delta moveDir mdl) msgs

-- Process an input message
stepMsg :: Msg -> Model -> Model
stepMsg msg model@Model { playerPos, playerDir, missiles } = case msg of
  Fire -> model { missiles = (playerPos, playerDir) : missiles }

-- Step the world forward by some small delta, using the input state
physics :: NominalDiffTime -> Vec -> Model -> Model
physics (realToFrac -> delta) movedir model@Model { .. } = model
  { playerPos = playerPos + (movedir ^* (delta * playerMovementSpeed))
  , playerDir = if vnull movedir then playerDir else movedir
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

-- ** RENDERING **

-- Load meshes, textures, materials, fonts, etc.
loadResources :: String -> H.VulkanResources -> Acquire ResourcesStore
loadResources path vulkanResources = do
  resourcesStore@ResourcesStore{..} <- withResourcesStore vulkanResources path
  liftIO do
    loadResource textures "circle.png" (FILTER_LINEAR, SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE)

    -- gidolinya.json (font data) and gidolinya.png (font texture) were
    -- generated using https://github.com/Chlumsky/msdf-atlas-gen
    loadResource fonts "gidolinya" 2

  pure resourcesStore

-- Our render function
renderGame :: (MonadIO m) => Resources -> Size Scalar -> Model -> (H.Renderer, H.FrameContext) -> m ()
renderGame res scrSize@(Size w _h) Model { playerPos, missiles } (renderer, frameContext)
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
  litF = flip runReaderT res do
    square <- getMesh "square"
    circleTex <- getTexture "circle.png"
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

  overlayF = flip runReaderT res do
    textRenderer <- getSomeFont
    H.drawText textRenderer (mkTranslation (topLeft 20 20 scrSize) !*! mkScale (V2 (12/12) (12/12)))
      white white 0 $ textcommand { text = "Arrow keys move, Space shoots", align = AlignLeft }

-- ** INPUT **

moveKeyVec :: Key -> Vec
moveKeyVec key = case key of
  Key'Up    -> V2 0 (-1)
  Key'Down  -> V2 0 1
  Key'Left  -> V2 (-1) 0
  Key'Right -> V2 1 0
  _ -> V2 0 0

physicsTimeStep :: NominalDiffTime
physicsTimeStep = 1/60

-- Build the FRP network
buildNetwork :: Resources -> CoreEventGenerators (H.Renderer, H.FrameContext) -> IO ()
buildNetwork res evGens = do
  B.actuate <=< B.compile $ mdo
    coreEvents <- mkCoreEvents evGens

    -- Input state
    let moveDir = fmap sum . sequenceA $
          (\k -> bool zero (moveKeyVec k) <$> keyHeldB coreEvents k)
          <$> ([ Key'Up, Key'Down, Key'Left, Key'Right ] :: [Key])

    -- Input events
    let inputs = unionAll
          [ Fire <$ B.filterE (==Key'Space) (keyDown coreEvents)
          ]

    -- Run the game network
    (mdl, _, _) <- gameNetwork
      physicsTimeStep -- Time interval for running the step function (may be less than rendering interval)
      Key'Escape -- Key to pause game for debugging
      coreEvents
      newGame -- Initial game state
      B.never -- Event to load a game state
      inputs -- Event containing inputs. Gathered every step interval and passed to step function.
      (stepF <$> moveDir) -- Game state step function

    -- every time we get a 'render' event tick, draw the screen
    B.reactimate
      (renderGame res <$> (fmap realToFrac <$> scrSizeB coreEvents) <*> mdl <@> eRender coreEvents)

main :: IO ()
main = GLFWV.withWindow 750 750 "Demo" \win -> runAcquire do
  vulkanResources <- initGLFWVulkan win
  resStore <- loadResources "Example/Shooter/assets/" vulkanResources
  -- setup event generators for core input (keys, mouse clicks, and elapsed time, etc.)
  (coreEvProc, evGens) <- liftIO $ glfwCoreEventGenerators win

  -- build and run the FRP network
  liftIO do
    res <- getResourcesStoreResources resStore
    buildNetwork res evGens

  liftIO $ GLFWV.runFrames win vulkanResources (H.withRenderer vulkanResources) \renderer frameContext -> do
    coreEvProc (renderer, frameContext)

    focused <- GLFW.getWindowFocused win
    -- don't consume CPU when the window isn't focused
    unless focused (threadDelay 100000)
