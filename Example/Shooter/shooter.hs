{- shooter.hs
 - This game has simple character movement with the arrow keys,
 - as well as shooting simple missiles with the SpaceBar.
 -}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Time.Clock (NominalDiffTime)
import Hickory.Camera
import Hickory.FRP.UI (topLeft)
import Hickory.Input
import Hickory.Math (vnull, mkTranslation, mkScale, viewTarget, Interpolatable (..))
import Hickory.Types
import Linear ( V2(..), V3(..), (^*), V4(..), (!*!), zero, identity)
import Linear.Metric
import qualified Graphics.UI.GLFW as GLFW
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
import Hickory.Vulkan.Forward.Types (OverlayGlobals(..), DrawCommand (..))
import Platforms.GLFW.Vulkan (initGLFWVulkan)
import Hickory.Resources (ResourcesStore(..), withResourcesStore, getMesh, getTexture, getResourcesStoreResources, Resources, getSomeFont, loadTextureResource, loadFontResource, runResources)
import Platforms.GLFW.Bridge (glfwFrameBuilder, getWindowSizeRef)
import Data.IORef (readIORef, IORef, newIORef, modifyIORef', atomicModifyIORef', writeIORef)
import qualified Data.Sequence as S
import Data.Maybe (fromMaybe)
import Data.Functor ((<&>))
import Ki
import qualified Data.Enum.Set as E
import GHC.Compact (compact, getCompact)
import qualified Data.Map.Strict as HashMap

-- ** GAMEPLAY **

type Vec = V2 Scalar

-- Our game data
data Model = Model
  { playerPos :: Vec
  , playerDir :: Vec
  , missiles  :: HashMap.Map Int (Vec, Vec)
  , nFired    :: Int
  }

instance Interpolatable Model where
  glerp fr a b =
    b
    { playerPos = glerp fr a.playerPos b.playerPos
    , playerDir = glerp fr a.playerDir b.playerDir
    , missiles  = HashMap.differenceWith (\y x -> Just $ glerp fr x y) b.missiles a.missiles
    }

-- Input messages
data Msg = Fire

-- By default, our firingDirection is to the right
newGame :: Model
newGame = Model (V2 0 0) (V2 0 0) mempty 0

-- Move our game state forward in time
stepF :: Vec -> (NominalDiffTime, [Msg]) -> Model -> (Model, [()])
stepF moveDir (delta, msgs) mdl = (,[]) $ foldr stepMsg (physics delta moveDir mdl) msgs

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
    loadTextureResource vulkanResources resourcesStore (path ++ "images/circle.png") (FILTER_LINEAR, SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE)

    -- gidolinya.json (font data) and gidolinya.png (font texture) were
    -- generated using https://github.com/Chlumsky/msdf-atlas-gen
    loadFontResource vulkanResources resourcesStore (path ++ "fonts/gidolinya.json") (path ++ "images/gidolinya.png") 2

  pure resourcesStore

nullRenderF :: MonadIO m => Size Scalar -> (H.Renderer, H.FrameContext) -> m ()
nullRenderF _ (renderer, frameContext) = H.renderToRenderer frameContext renderer renderSettings (pure ()) (pure ())
  where
  renderSettings = H.RenderSettings
    { worldSettings = H.worldSettingsDefaults
    , overlayGlobals = OverlayGlobals
      { viewMat = identity
      , projMat = identity
      , viewProjMat = identity
      }
    , postSettings = H.postDefaults
    , clearColor = V4 0 0 0 1
    , highlightObjs = []
    }

-- Our render function
renderGame :: (MonadIO m) => Resources -> Model -> Size Scalar -> (H.Renderer, H.FrameContext) -> m ()
renderGame res Model { playerPos, missiles } scrSize@(Size w _h) (renderer, frameContext)
  = H.renderToRenderer frameContext renderer renderSettings litF overlayF
  where
  vm = viewTarget (V3 0 0 (-1)) (V3 0 0 1) (V3 0 (-1) 0)
  pm = shotMatrix (Ortho w 1 100 False) (aspectRatio scrSize)
  renderSettings = H.RenderSettings
    { worldSettings = H.worldSettingsDefaults
      { H.camera = Camera (V3 0 0 (-1)) (V3 0 0 1) (V3 0 (-1) 0) (Ortho w 1 100 True)
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

  overlayF = runResources res do
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
physicsTimeStep = 1/20

mkMoveDir :: InputFrame -> Vec
mkMoveDir InputFrame {..} = sum
  (([ Key'Up, Key'Down, Key'Left, Key'Right ] <&> \k -> if E.member k heldKeys || E.member k pressedKeys then moveKeyVec k else zero) :: [Vec])

general
  :: forall model b. Interpolatable model
  => GLFW.Window
  -> H.VulkanResources
  -> (InputFrame -> model -> (model, b))
  -> (model -> Size Scalar -> (H.Renderer, H.FrameContext) -> IO ())
  -> model
  -> IO ()
general win vulkanResources stepFunction renderF initialModel = do
  frameBuilder <- glfwFrameBuilder win
  scrSizeRef <- getWindowSizeRef win
  inputRef    :: IORef InputFrame <- newIORef mempty
  stateRef    :: IORef (S.Seq model) <- newIORef $ S.singleton initialModel
  renderFRef  :: IORef (Scalar -> Size Scalar -> (H.Renderer, H.FrameContext) -> IO ()) <- newIORef (const nullRenderF)
  stateIdxRef :: IORef Int <- newIORef 0
  Ki.scoped \scope -> do
    _thr <- Ki.fork scope do
      forever do
        batched <- atomicModifyIORef' inputRef \cur ->
          let timeRemaining = physicsTimeStep - cur.delta
          in if timeRemaining > 0
              then (cur, Left timeRemaining)
              else (mempty { heldKeys = cur.heldKeys }, Right cur { delta = physicsTimeStep })
        case batched of
          Left timeRemaining -> threadDelay (ceiling @Double $ realToFrac timeRemaining * 1000000)
          Right inputFrame -> do
            lastState <- readIORef stateRef <&> fromMaybe (error "No game states available") . S.lookup 0
            newState <- fmap getCompact . compact . fst $ stepFunction inputFrame lastState
            modifyIORef' stateRef (\s -> S.take 500 $ newState S.<| s)
            let interpolated frac = glerp frac lastState newState
            writeIORef renderFRef (renderF . interpolated)

      -- mdl <- ((,) <$> readIORef stateRef <*> readIORef stateIdxRef) <&> \(gameSeq, idx) ->
      --   let idx' = min idx (S.length gameSeq - 2)
      --   in case (,) <$> S.lookup idx' gameSeq <*> S.lookup (idx'+1) gameSeq of
      --       Just (to, from) -> glerp (realToFrac $ curInputFrame.delta / physicsTimeStep) from to
      --       Nothing -> fromMaybe (error "No game states available") $ S.lookup 0 gameSeq

    GLFWV.runFrames win vulkanResources (H.withRenderer vulkanResources) \renderer frameContext -> do
      inputFrame <- frameBuilder
      modifyIORef' inputRef (inputFrame<>)
      curInputFrame <- readIORef inputRef

      scrSize <- readIORef scrSizeRef
      readIORef renderFRef >>= \f -> f (realToFrac $ curInputFrame.delta / physicsTimeStep) (realToFrac <$> scrSize) (renderer, frameContext)

      focused <- GLFW.getWindowFocused win
      -- don't consume CPU when the window isn't focused
      unless focused (threadDelay 100000)

main :: IO ()
main = GLFWV.withWindow 750 750 "Demo" \win -> runAcquire do
  vulkanResources <- initGLFWVulkan win
  resStore <- loadResources "Example/Shooter/assets/" vulkanResources
  liftIO do
    res <- getResourcesStoreResources resStore

    let steppper inputFrame = stepF (mkMoveDir inputFrame) (inputFrame.delta, msgs)
          where
          msgs = [ Fire | E.member Key'Space inputFrame.pressedKeys ]
    general win vulkanResources steppper (renderGame res) newGame
