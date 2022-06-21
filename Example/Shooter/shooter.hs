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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class ( MonadIO, MonadIO(liftIO) )
import Control.Monad.Reader (MonadReader, runReaderT, ask)
import Data.Maybe (maybeToList)
import Data.Time.Clock (NominalDiffTime)
import Hickory.Camera
import Hickory.FRP.CoreEvents (mkCoreEvents, CoreEvents(..), CoreEventGenerators)
import Hickory.FRP.Game (timeStep)
import Hickory.FRP.UI (topLeft)
import Hickory.FRP.Historical (historical)
import Hickory.Input
import Hickory.Math (vnull, mkTranslation, mkScale)
import Hickory.Types
import Linear ( V2(..), V3(..), (^*), M44, V3(..), (!*!), transpose )
import Linear.Metric
import Platforms.GLFW.FRP (glfwCoreEventGenerators)
import Reactive.Banana ((<@>))
import qualified Graphics.UI.GLFW as GLFW
import qualified Hickory.Graphics as H
import qualified Reactive.Banana as B
import qualified Reactive.Banana.Frameworks as B

import qualified Platforms.GLFW.Vulkan as GLFWV
import qualified Hickory.Vulkan.Vulkan as H

import Control.Monad
import Control.Monad.Managed (Managed, runManaged)
import Vulkan
  ( ShaderStageFlagBits (..)
  , CommandBuffer(..)
  , deviceWaitIdle
  )

import qualified Data.ByteString as B
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag, vert)
import Control.Monad.Extra (whenM)

import Hickory.Vulkan.Vulkan
import qualified Hickory.Vulkan.Mesh as H
import qualified Hickory.Vulkan.Material as H
import Data.Proxy (Proxy(..))
import Data.Foldable (for_)

-- ** GAMEPLAY **

type Vec = V2 Double

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
playerMovementSpeed :: Double
playerMovementSpeed = 100

missileMovementSpeed :: Double
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
  { square         :: H.BufferedMesh
  , solidMaterial  :: H.Material
  , circleMaterial :: H.Material
  -- , printer     :: H.Printer Int -- For text rendering
  }

-- Set up our scene, load assets, etc.
loadResources :: Bag -> String -> Managed Resources
loadResources bag path = do
  square <- H.withBufferedMesh bag $ H.Mesh
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
    , indices = Just [0, 1, 2, 2, 3, 0]
    }
  solidMaterial  <- H.withMaterial bag [(Proxy @(M44 Float), SHADER_STAGE_VERTEX_BIT)] (H.meshAttributes . H.mesh $ square) vertShader fragShader []
  circleMaterial <- H.withMaterial bag [(Proxy @(M44 Float), SHADER_STAGE_VERTEX_BIT)] (H.meshAttributes . H.mesh $ square) vertShader texFragShader [path ++ "/images/circle.png"]

  {-
  -- A shader for drawing text
  pvcshader <- H.loadPVCShader shaderVersion

  -- gidolinya.fnt (font data) and gidolinya.png (font texture) were
  -- generated using the bmGlyph program for Mac
  pr <- H.loadPrinter path pvcshader "gidolinya"
  -}

  pure $ Resources {..}

-- Our render function
renderGame :: (MonadIO m, MonadReader Resources m) => Size Double -> Model -> NominalDiffTime -> CommandBuffer -> m ()
renderGame scrSize Model { playerPos, missiles } _gameTime commandBuffer = do
  Resources {..} <- ask
  H.runMatrixT . H.xform (gameCameraMatrix scrSize) $ do
    for_ missiles \(pos, _) -> H.xform (mkTranslation pos !*! mkScale (V2 5 5)) do
      mat :: M44 Float <- fmap (fmap realToFrac) . transpose <$> H.askMatrix
      H.cmdBindMaterial commandBuffer circleMaterial
      H.cmdPushMaterialConstants commandBuffer circleMaterial SHADER_STAGE_VERTEX_BIT mat
      H.cmdDrawBufferedMesh commandBuffer square
    H.xform (mkTranslation playerPos !*! mkScale (V2 10 10)) do
      mat :: M44 Float <- fmap (fmap realToFrac) . transpose <$> H.askMatrix
      H.cmdBindMaterial commandBuffer solidMaterial
      H.cmdPushMaterialConstants commandBuffer solidMaterial SHADER_STAGE_VERTEX_BIT mat
      H.cmdDrawBufferedMesh commandBuffer square

  H.runMatrixT . H.xform (uiCameraMatrix scrSize) $ do
    H.xform (mkTranslation (topLeft 20 20 scrSize)) do
      pure ()
      -- H.drawText $ H.textcommand { H.color = white, H.text = "Arrow keys move, Space shoots", H.align = H.AlignLeft, H.fontSize = 5 }

  where
  gameCameraMatrix size@(Size w _h) =
    let proj = Ortho w 1 100 True
        camera = Camera proj (V3 0 0 10) (V3 0 0 0) (V3 0 1 0)
    in viewProjectionMatrix camera (aspectRatio size)
  uiCameraMatrix size@(Size w _h) =
    let proj = Ortho w (-20) 1 False
        camera = Camera proj (V3 0 0 0) (V3 0 0 (-1)) (V3 0 1 0)
    in viewProjectionMatrix camera (aspectRatio size)

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
buildNetwork :: Resources -> CoreEventGenerators CommandBuffer -> IO ()
buildNetwork resources evGens = do
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
    B.reactimate $ flip runReaderT resources <$>
      (renderGame <$> (fmap realToFrac <$> scrSizeB coreEvents) <*> mdl <*> currentTime <@> eRender coreEvents)

main :: IO ()
main = GLFWV.withWindow 750 750 "Demo" $ \win bag@H.Bag {..} -> do
  let H.DeviceContext {..} = deviceContext
  runManaged do
    resources <- loadResources bag "Example/Shooter/assets"
    liftIO do
      -- setup event generators for core input (keys, mouse clicks, and elapsed time, etc.)
      (coreEvProc, evGens) <- glfwCoreEventGenerators win

      -- build and run the FRP network
      buildNetwork resources evGens

      let loop frameNumber = do
            -- check the input buffers and generate FRP events
            H.drawFrame frameNumber bag coreEvProc

            focused <- GLFW.getWindowFocused win

            -- don't consume CPU when the window isn't focused
            unless focused (threadDelay 100000)
            whenM (not <$> GLFW.windowShouldClose win) $ loop (frameNumber + 1)
      loop 0
      deviceWaitIdle device

vertShader :: B.ByteString
vertShader = [vert|
  #version 450

  layout(location = 0) in vec3 inPosition;
  layout(location = 3) in vec2 inTexCoord;

  layout(location = 1) out vec2 texCoord;

  layout( push_constant ) uniform constants
  {
    mat4 modelViewMatrix;
  } PushConstants;

  void main() {
      gl_Position = PushConstants.modelViewMatrix * vec4(inPosition, 1.0);
      texCoord = inTexCoord;
  }

|]

fragShader :: B.ByteString
fragShader = [frag|
  #version 450

  layout(location = 1) in vec2 texCoord;

  layout(location = 0) out vec4 outColor;

  void main() {
    outColor = vec4(1.0);
  }

|]

texFragShader :: B.ByteString
texFragShader = [frag|
  #version 450

  layout(location = 1) in vec2 texCoord;

  layout(binding = 0) uniform sampler2D texSampler;

  layout(location = 0) out vec4 outColor;

  void main() {
    outColor = texture(texSampler, texCoord);
  }

|]
