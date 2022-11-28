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
{-# LANGUAGE TypeApplications #-}
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
import qualified Hickory.Graphics as H
import qualified Reactive.Banana as B
import qualified Reactive.Banana.Frameworks as B
import Control.Lens (view)
import Acquire.Acquire (Acquire)
import qualified Data.ByteString.Lazy as BS

import qualified Platforms.GLFW.Vulkan as GLFWV
import qualified Hickory.Vulkan.Text as H

import Control.Monad
import Vulkan
  ( pattern FILTER_LINEAR, Instance
  , SamplerAddressMode(..)
  )

import qualified Data.ByteString as B
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag, vert)

import Hickory.Vulkan.Vulkan
import qualified Hickory.Vulkan.Mesh as H
import qualified Hickory.Vulkan.DescriptorSet as H
import Hickory.Vulkan.Monad (drawMesh, useDynamicMesh, drawText)
import Data.Foldable (for_)
import Foreign.Storable.Generic
import Hickory.Graphics.DrawText (textcommand)
import Hickory.Text (TextCommand(..), Font, makeFont, XAlign (..))
import Hickory.Math.Vector (Scalar)
import Hickory.Vulkan.Framing (FramedResource, frameResource, resourceForFrame)
import qualified Hickory.Vulkan.Monad as H
import Hickory.Vulkan.Frame (FrameContext, frameNumber)
import qualified Hickory.Vulkan.RenderPass as H
import qualified Hickory.Vulkan.DynamicMesh as H
import Hickory.Color (white)

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
  { target              :: H.RenderTarget
  , circleTex           :: H.PointedDescriptorSet
  , fontTex             :: H.PointedDescriptorSet
  , square              :: H.BufferedMesh
  , solidMaterial       :: H.BufferedUniformMaterial SolidColorUniform
  , texturedMaterial    :: H.BufferedUniformMaterial TextureUniform
  , msdfMaterial        :: H.BufferedUniformMaterial H.MSDFMatConstants
  , font                :: Font
  , dynamicMesh         :: FramedResource H.DynamicBufferedMesh
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
  target@H.RenderTarget {..} <- H.withStandardRenderTarget vulkanResources swapchain
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
  solidMaterial    <- H.withBufferedUniformMaterial vulkanResources swapchain renderPass
    [H.Position, H.TextureCoord] vertShader fragShader Nothing Nothing
  texturedMaterial <- H.withBufferedUniformMaterial vulkanResources swapchain renderPass
    [H.Position, H.TextureCoord] texVertShader texFragShader Nothing (Just circleTex)
  msdfMaterial <- H.withMSDFMaterial vulkanResources swapchain renderPass fontTex

  -- gidolinya.json (font data) and gidolinya.png (font texture) were
  -- generated using https://github.com/Chlumsky/msdf-atlas-gen
  text <- liftIO $ BS.readFile $ path ++ "fonts/gidolinya.json"
  let font = case makeFont text of
                Left s -> error s
                Right f -> f

  -- We need a dynamic buffer per frame to write text mesh data
  dynamicMesh <- frameResource $ H.withDynamicBufferedMesh vulkanResources 1000

  pure $ Resources {..}

-- Our render function
renderGame :: MonadIO m => Size Scalar -> Model -> NominalDiffTime -> (Resources, FrameContext) -> m ()
renderGame scrSize Model { playerPos, missiles } _gameTime (Resources {..}, frameContext)
  = H.runFrame frameContext
  . H.runBatchIO
  . useDynamicMesh (resourceForFrame (frameNumber frameContext) dynamicMesh)
  $ H.renderToTarget target (V4 0 0 0 1) (H.PostConstants 0 (V3 1 1 1) 1 0 (frameNumber frameContext)) litF overlayF
  where
  litF = do
    H.runMatrixT . H.xform (gameCameraMatrix scrSize) $ do
      for_ missiles \(pos, _) -> H.xform (mkTranslation pos !*! mkScale (V2 5 5)) do
        mat <- H.askMatrix
        drawMesh True False texturedMaterial (TextureUniform mat) square (Just circleTex)

      H.xform (mkTranslation playerPos !*! mkScale (V2 10 10)) do
        mat <- H.askMatrix
        drawMesh False False solidMaterial (SolidColorUniform mat (V4 1 0 0 1)) square Nothing

  overlayF = do
    H.runMatrixT . H.xform (uiCameraMatrix scrSize) $ do
      H.xform (mkTranslation (topLeft 20 20 scrSize) !*! mkScale (V2 (12/12) (12/12))) do
        mat <- H.askMatrix
        drawText msdfMaterial (H.MSDFMatConstants mat white white 0 2 (V2 1 1)) font (textcommand { text = "Arrow keys move, Space shoots", align = AlignLeft } ) fontTex

  gameCameraMatrix size@(Size w _h) =
    let proj = Ortho w 1 100 True
    in shotMatrix proj (aspectRatio size) !*! viewTarget (V3 0 0 (-1)) (V3 0 0 1) (V3 0 (-1) 0)
  uiCameraMatrix size@(Size w _h) =
    let proj = Ortho w 1 100 False
    in shotMatrix proj (aspectRatio size) !*! viewTarget (V3 0 0 (-1)) (V3 0 0 1) (V3 0 (-1) 0)

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

vertShader :: B.ByteString
vertShader = [vert|
  #version 450

  layout(location = 0) in vec3 inPosition;

  struct Uniforms
  {
    mat4 modelViewMatrix;
    vec4 color;
  };

  layout (row_major, std140, set = 0, binding = 0) uniform UniformBlock {
    Uniforms uniforms [128];
  } ub;

  layout( push_constant ) uniform constants
  {
    int uniformIdx;
  } PushConstants;

  void main() {
      Uniforms uniforms = ub.uniforms[PushConstants.uniformIdx];
      gl_Position = uniforms.modelViewMatrix * vec4(inPosition, 1.0);
  }

|]

fragShader :: B.ByteString
fragShader = [frag|
  #version 450

  layout(location = 0) out vec4 outColor;

  struct Uniforms
  {
    mat4 modelViewMatrix;
    vec4 color;
  };

  layout (row_major, std140, set = 0, binding = 0) uniform UniformBlock {
    Uniforms uniforms [128];
  } ub;

  layout( push_constant ) uniform constants
  {
    int uniformIdx;
  } PushConstants;

  void main() {
    Uniforms uniforms = ub.uniforms[PushConstants.uniformIdx];
    outColor = uniforms.color;
  }

|]

texVertShader :: B.ByteString
texVertShader = [vert|
  #version 450
  #extension GL_EXT_scalar_block_layout : require

  layout(location = 0) in vec3 inPosition;
  layout(location = 3) in vec2 inTexCoord;

  layout(location = 1) out vec2 texCoord;

  struct Uniforms
  {
    mat4 modelViewMatrix;
  };

  layout (row_major, scalar, set = 0, binding = 0) uniform UniformBlock {
    Uniforms uniforms [128];
  } ub;

  layout( push_constant ) uniform constants
  {
    int uniformIdx;
  } PushConstants;

  void main() {
      Uniforms uniforms = ub.uniforms[PushConstants.uniformIdx];
      gl_Position = uniforms.modelViewMatrix * vec4(inPosition, 1.0);
      texCoord = inTexCoord;
  }

|]

texFragShader :: B.ByteString
texFragShader = [frag|
  #version 450
  #extension GL_EXT_scalar_block_layout : require
  #extension GL_EXT_nonuniform_qualifier : require

  layout(location = 1) in vec2 texCoord;

  layout(set = 1, binding = 0) uniform sampler2D texSampler;

  layout(location = 0) out vec4 outColor;

  struct Uniforms
  {
    mat4 modelViewMatrix;
  };

  layout (row_major, scalar, set = 0, binding = 0) uniform UniformBlock {
    Uniforms uniforms [128];
  } ub;

  layout( push_constant ) uniform constants
  {
    int uniformIdx;
  } PushConstants;

  void main() {
    Uniforms uniforms = ub.uniforms[PushConstants.uniformIdx];
    outColor = texture(texSampler, texCoord);
  }

|]
