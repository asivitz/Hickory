{- shooter.hs
 - This game has simple character movement with the arrow keys,
 - as well as shooting simple missiles with the SpaceBar.
 -}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Time.Clock (NominalDiffTime)
import Hickory.Camera
import Hickory.FRP.UI (topLeft)
import Hickory.Input
import Hickory.Math (vnull, mkTranslation, mkScale, viewTarget, Interpolatable (..))
import Hickory.Types
import Linear ( V2(..), V3(..), (^*), V4(..), (!*!), zero, transpose, _m33, inv33)
import Linear.Metric
import Acquire.Acquire (Acquire)
import Control.Lens ((^.))
import Foreign (poke)
import Control.Monad (void)

import qualified Platforms.GLFW.Vulkan as GLFWV
import qualified Hickory.Vulkan.Types as H
import qualified Hickory.Vulkan.Renderer.Types as H
import qualified Hickory.Vulkan.Renderer.Renderer as H

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
import Hickory.Vulkan.Renderer.Types (OverlayGlobals(..), DrawCommand (..))
import Platforms.GLFW.Vulkan (initGLFWVulkan)
import Hickory.Resources (ResourcesStore(..), withResourcesStore, getMesh, getTexture, getResourcesStoreResources, Resources, getSomeFont, loadTextureResource, loadFontResource, runResources)
import Data.Functor ((<&>))
import qualified Data.Enum.Set as E
import qualified Data.Map.Strict as HashMap
import Platforms.GLFW.GameLoop (gameLoop)
import Hickory.GameLoop (newGameStateStack, stepGameState, queryGameState)
import Data.IORef (newIORef, atomicModifyIORef', readIORef)

-- ** GAMEPLAY **

type Vec = V2 Scalar

-- Input messages
data Msg = Fire

-- Our game data
data Model = Model
  { playerPos :: Vec
  , playerDir :: Vec
  , missiles  :: HashMap.Map Int (Vec, Vec) -- Map from ids to a (pos,dir) pair
  , nFired    :: Int
  }

-- Our physics thread runs at 20fps while we render at a much higher rate than that
-- Therefore, in order for rendering to appear smooth, we need to
-- interpolate between the latest frame and the previous frame
-- This typeclass accomplishes that
instance Interpolatable Model where
  glerp fr a b =
    b
    { playerPos = glerp fr a.playerPos b.playerPos
    , playerDir = glerp fr a.playerDir b.playerDir
    -- The ids allow us to interpolate between missile states
    , missiles  = HashMap.differenceWith (\y x -> Just $ glerp fr x y) b.missiles a.missiles
    }

-- By default, our firingDirection is to the right
newGame :: Model
newGame = Model (V2 0 0) (V2 0 0) mempty 0

-- Move our game state forward in time
stepF :: InputFrame -> Model -> (Model, [()])
stepF inputFrame mdl = (actionStep,[])
  where
  actionStep = foldr stepMsg simulationStep msgs
  simulationStep = physics inputFrame.delta (mkMoveDir inputFrame) mdl
  msgs = [ Fire | E.member Key'Space inputFrame.pressedKeys ]

-- Process an input message
stepMsg :: Msg -> Model -> Model
stepMsg msg model@Model { playerPos, playerDir, missiles, nFired } = case msg of
  Fire -> model { nFired = nFired + 1, missiles = HashMap.insert nFired (playerPos, playerDir) missiles }

-- Step the world forward by some small delta, using the input state
physics :: NominalDiffTime -> Vec -> Model -> Model
physics (realToFrac -> delta) movedir model@Model { .. } = model
  { playerPos = playerPos + (movedir ^* (delta * playerMovementSpeed))
  , playerDir = if vnull movedir then playerDir else movedir
  , missiles = HashMap.filter missileInBounds $ flip HashMap.map missiles
      (\(pos, dir) -> (pos + (dir ^* (delta * missileMovementSpeed)), dir))
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
  resourcesStore <- withResourcesStore vulkanResources
  liftIO do
    loadTextureResource vulkanResources resourcesStore (path ++ "images/circle.png") (FILTER_LINEAR, SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE, Nothing)

    -- gidolinya.json (font data) and gidolinya.png (font texture) were
    -- generated using https://github.com/Chlumsky/msdf-atlas-gen
    loadFontResource vulkanResources resourcesStore (path ++ "fonts/gidolinya.json") (path ++ "images/gidolinya.png") 2

  pure resourcesStore

-- Our render function
renderGame :: (MonadIO m) => Resources -> Model -> Size Int -> (H.Renderer, H.FrameContext) -> m ()
renderGame res Model { playerPos, missiles } scrSize@(Size w _h) (renderer, frameContext)
  = void $ H.renderToRenderer frameContext renderer renderSettings litF overlayF
  where
  vm = viewTarget (V3 0 0 (-1)) (V3 0 0 1) (V3 0 (-1) 0)
  pm = shotMatrix (Ortho (realToFrac w) 1 100 False) (aspectRatio scrSize)
  renderSettings = H.RenderSettings
    { worldSettings = H.worldSettingsDefaults
      { H.camera = Camera (V3 0 0 (-1)) (V3 0 0 1) (V3 0 (-1) 0) (Ortho (realToFrac w) 1 100 True) "Main"
      }
    , overlayGlobals = OverlayGlobals
      { viewMat = vm
      , projMat = pm
      , viewProjMat = pm !*! vm
      }
    , postSettings = H.postDefaults
    , clearColor = V4 0 0 0 1
    , highlightObjs = []
    }
  litF = runResources res do
    square <- getMesh "square"
    circleTex <- getTexture "circle.png"
    for_ missiles \(pos, _) -> do
      let mat = mkTranslation pos !*! mkScale (V2 5 5)
      H.addCommand $ DrawCommand
        { instances = [(0,mat)]
        , mesh = H.Buffered square
        , pokeData = flip poke $ H.StaticConstants
            { modelMat    = mat
            , normalMat   = transpose . inv33 $ mat ^. _m33
            , color       = red
            , specularity = 1
            , tiling      = V2 1 1
            }
        , cull = False
        , doCastShadow = False
        , doBlend = True
        , descriptorSet = Just circleTex
        , materialConfig = renderer.staticDirectMaterialConfig
        }

    let mat = mkTranslation playerPos !*! mkScale (V2 10 10)
    H.addCommand $ DrawCommand
      { instances = [(0,mat)]
      , mesh = H.Buffered square
      , pokeData = flip poke $ H.StaticConstants
          { modelMat    = mat
          , normalMat   = transpose . inv33 $ mat ^. _m33
          , color       = white
          , specularity = 1
          , tiling      = V2 1 1
          }
      , cull = False
      , doCastShadow = False
      , doBlend = False
      , descriptorSet = Just circleTex
      , materialConfig = renderer.staticDirectMaterialConfig
      }

  overlayF = runResources res do
    textRenderer <- getSomeFont
    H.drawText renderer.msdfMaterialConfig textRenderer (mkTranslation (topLeft 20 20 scrSize) !*! mkScale (V2 (12/12) (12/12)))
      white white 0 $ textcommand { text = "Arrow keys move, Space shoots", align = AlignLeft }

-- ** INPUT **

mkMoveDir :: InputFrame -> Vec
mkMoveDir InputFrame {..} = sum
  (([ Key'Up, Key'Down, Key'Left, Key'Right ] <&> \k -> if E.member k heldKeys || E.member k pressedKeys then moveKeyVec k else zero) :: [Vec])
  where
  moveKeyVec :: Key -> Vec
  moveKeyVec key = case key of
    Key'Up    -> V2 0 (-1)
    Key'Down  -> V2 0 1
    Key'Left  -> V2 (-1) 0
    Key'Right -> V2 1 0
    _ -> V2 0 0

physicsTimeStep :: NominalDiffTime
physicsTimeStep = 1/20

main :: IO ()
main = GLFWV.withWindow 750 750 "Demo" \win -> runAcquire do
  vulkanResources <- initGLFWVulkan win
  resStore <- loadResources "Example/Shooter/assets/" vulkanResources
  liftIO do
    res <- getResourcesStoreResources resStore

    let mkScene = do
          stack <- newIORef (newGameStateStack newGame)
          pure \inputFrame -> do
            _evs <- atomicModifyIORef' stack (stepGameState $ stepF inputFrame)
            pure $ \frac _ size (sr,fc) -> do
              model <- queryGameState <$> readIORef stack
              renderGame res (model (2 - frac)) size (sr, fc)

    gameLoop win vulkanResources (H.withRenderer vulkanResources) physicsTimeStep $ const mkScene
