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

import GHC.Generics (Generic)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class ( MonadIO, MonadIO(liftIO) )
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
import Linear ( V2(..), V3(..), (^*), M44, V4(..), (!*!))
import Linear.Metric
import Platforms.GLFW.FRP (glfwCoreEventGenerators)
import Reactive.Banana ((<@>))
import qualified Graphics.UI.GLFW as GLFW
import qualified Reactive.Banana as B
import qualified Reactive.Banana.Frameworks as B
import Control.Lens (view)
import Acquire.Acquire (Acquire)
import qualified Data.ByteString.Lazy as BS

import qualified Platforms.GLFW.Vulkan as GLFWV
import qualified Hickory.Vulkan.Types as H
import qualified Hickory.Vulkan.Forward.Types as H
import qualified Hickory.Vulkan.Forward.Renderer as H

import Control.Monad
import Vulkan
  ( pattern FILTER_LINEAR, Instance
  , SamplerAddressMode(..)
  )

import Hickory.Vulkan.Vulkan
import qualified Hickory.Vulkan.Mesh as H
import qualified Hickory.Vulkan.DescriptorSet as H
import Hickory.Vulkan.Monad (textMesh)
import Data.Foldable (for_)
import Foreign.Storable.Generic
import Hickory.Graphics.DrawText (textcommand)
import Hickory.Text (TextCommand(..), Font, makeFont, XAlign (..))
import Hickory.Math.Vector (Scalar)
import qualified Hickory.Vulkan.Monad as H
import Hickory.Vulkan.Frame (FrameContext, frameNumber)
import Hickory.Color (white, red)
import Hickory.Vulkan.Forward.Types (WorldGlobals(..), OverlayGlobals(..), DrawCommand (..))

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
  , fontTex             :: H.PointedDescriptorSet
  , square              :: H.BufferedMesh
  -- , solidMaterial       :: H.BufferedUniformMaterial SolidColorUniform
  -- , texturedMaterial    :: H.BufferedUniformMaterial TextureUniform
  -- , msdfMaterial        :: H.BufferedUniformMaterial H.MSDFMatConstants
  , font                :: Font
  -- , dynamicMesh         :: FramedResource H.DynamicBufferedMesh
  , renderer            :: H.Renderer
  }

data SolidColorUniform = SolidColorUniform
  { mat   :: M44 Scalar
  , color :: V4 Scalar
  } deriving Generic
    deriving anyclass GStorable

newtype TextureUniform = TextureUniform
  { mat   :: M44 Scalar
  } deriving Generic
    deriving anyclass GStorable

-- Load meshes, textures, materials, fonts, etc.
loadResources :: String -> Size Int -> Instance -> VulkanResources -> Swapchain -> Acquire Resources
loadResources path _size _inst vulkanResources swapchain = do
  circleTex <- view #descriptorSet <$> H.withTextureDescriptorSet vulkanResources [(path ++ "images/circle.png", FILTER_LINEAR, SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE)]
  fontTex   <- view #descriptorSet <$> H.withTextureDescriptorSet vulkanResources [(path ++ "images/gidolinya.png", FILTER_LINEAR, SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE)]

  square <- H.withBufferedMesh vulkanResources $ H.Mesh
    { vertices =
          [ (H.Position, [ -0.5, -0.5, 0.0
                         ,  0.5, -0.5, 0.0
                         ,  0.5,  0.5, 0.0
                         , -0.5,  0.5, 0.0
                         ])
          , (H.TextureCoord, [ 0.0, 0.0
                             , 1.0, 0.0
                             , 1.0, 1.0
                             , 0.0, 1.0
                             ])
          ]
    , indices = Just [0, 2, 1, 2, 0, 3]
    }

  -- gidolinya.json (font data) and gidolinya.png (font texture) were
  -- generated using https://github.com/Chlumsky/msdf-atlas-gen
  text <- liftIO $ BS.readFile $ path ++ "fonts/gidolinya.json"
  let font = case makeFont text of
                Left s -> error s
                Right f -> f

  renderer <- H.withRenderer vulkanResources swapchain

  pure $ Resources {..}

-- Our render function
renderGame :: MonadIO m => Size Scalar -> Model -> NominalDiffTime -> (Resources, FrameContext) -> m ()
renderGame scrSize@(Size w _h) Model { playerPos, missiles } _gameTime (Resources {..}, frameContext)
  = H.runFrame frameContext
  $ H.renderToRenderer renderer renderSettings (H.PostConstants 0 (V3 1 1 1) 1 0 (frameNumber frameContext)) litF overlayF
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
    , clearColor = V4 0 0 0 1
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
        }

    H.addCommand $ DrawCommand
      { modelMat = mkTranslation playerPos !*! mkScale (V2 10 10)
      , mesh = H.Buffered square
      , color = red
      , drawType = H.Static $ H.StaticMesh circleTex (V2 1 1)
      , lit = False
      , castsShadow = False
      , blend = False
      }

  overlayF = do
    let tc = textcommand { text = "Arrow keys move, Space shoots", align = AlignLeft }
    H.addCommand $ DrawCommand
      { modelMat = mkTranslation (topLeft 20 20 scrSize) !*! mkScale (V2 (12/12) (12/12))
      , mesh = H.Dynamic (textMesh font tc)
      , color = white
      , drawType = H.MSDF $ H.MSDFMesh fontTex white 0 2 (V2 1 1)
      , lit = False
      , castsShadow = False
      , blend = False
      }

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
buildNetwork :: CoreEventGenerators (Resources, FrameContext) -> IO ()
buildNetwork evGens = do
  B.actuate <=< B.compile $ mdo
    coreEvents <- mkCoreEvents evGens

    -- currentTime isn't currently used, but it's useful for things like animation
    currentTime <- B.accumB 0 ((+) <$> eTime coreEvents)

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
      (renderGame <$> (fmap realToFrac <$> scrSizeB coreEvents) <*> mdl <*> currentTime <@> eRender coreEvents)

main :: IO ()
main = GLFWV.withWindow 750 750 "Demo" \win -> do
  -- setup event generators for core input (keys, mouse clicks, and elapsed time, etc.)
  (coreEvProc, evGens) <- glfwCoreEventGenerators win

  -- build and run the FRP network
  buildNetwork evGens

  GLFWV.runFrames win (loadResources "Example/Shooter/assets/") \resources frameContext -> do
    coreEvProc (resources, frameContext)

    focused <- GLFW.getWindowFocused win
    -- don't consume CPU when the window isn't focused
    unless focused (threadDelay 100000)
