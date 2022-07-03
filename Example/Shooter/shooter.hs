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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

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
import Hickory.Math (vnull, mkTranslation, mkScale)
import Hickory.Types
import Linear ( V2(..), V3(..), (^*), M44, V4(..), (!*!), transpose )
import Linear.Metric
import Platforms.GLFW.FRP (glfwCoreEventGenerators)
import Reactive.Banana ((<@>))
import qualified Graphics.UI.GLFW as GLFW
import qualified Hickory.Graphics as H
import qualified Reactive.Banana as B
import qualified Reactive.Banana.Frameworks as B

import qualified Platforms.GLFW.Vulkan as GLFWV
import qualified Hickory.Vulkan.Text as H

import Control.Monad
import Control.Monad.Managed (Managed)
import Vulkan
  ( CommandBuffer(..)
  , PrimitiveTopology (..)
  )

import qualified Data.ByteString as B
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag, vert)

import Hickory.Vulkan.Vulkan
import qualified Hickory.Vulkan.Mesh as H
import qualified Hickory.Vulkan.Material as H
import Hickory.Vulkan.Monad (targetCommandBuffer, useMaterial, pushConstant, draw, useDynamicMesh, drawText)
import Data.Foldable (for_)
import Foreign.Storable.Generic
import Hickory.Graphics.DrawText (textcommand)
import Hickory.Text.Text (TextCommand(..), Font, makeFont, XAlign (..))
import Hickory.Utils.Utils (readFileAsText)
import Data.Either (fromRight)
import Hickory.Color (white)
import Hickory.Math.Vector (Scalar)

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
  { square         :: H.BufferedMesh
  , solidMaterial  :: H.Material SolidColorPushConstant
  , circleMaterial :: H.Material (M44 Scalar)
  , textMaterial   :: H.Material (M44 Scalar)
  , font           :: Font Int
  }

data SolidColorPushConstant = SolidColorPushConstant
  { mat   :: M44 Scalar
  , color :: V4 Scalar
  } deriving Generic
    deriving anyclass GStorable

-- Load meshes, textures, materials, fonts, etc.
loadResources :: String -> Size Int -> VulkanResources -> SwapchainContext -> Managed Resources
loadResources path _size vulkanResources swapchainContext = do
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
    , indices = Just [0, 1, 2, 2, 3, 0]
    }
  solidMaterial  <- H.withMaterial @SolidColorPushConstant vulkanResources swapchainContext
    [H.Position, H.TextureCoord] PRIMITIVE_TOPOLOGY_TRIANGLE_LIST vertShader fragShader []
  circleMaterial <- H.withMaterial @(M44 Scalar) vulkanResources swapchainContext
    [H.Position, H.TextureCoord] PRIMITIVE_TOPOLOGY_TRIANGLE_LIST texVertShader texFragShader [path ++ "/images/circle.png"]
  textMaterial <- H.withMaterial @(M44 Scalar) vulkanResources swapchainContext
    [H.Position, H.TextureCoord, H.Color] PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP textVertShader textFragShader [path ++ "/images/gidolinya.png"]

  -- gidolinya.fnt (font data) and gidolinya.png (font texture) were
  -- generated using the bmGlyph program for Mac
  text <- liftIO $ readFileAsText $ path ++ "/fonts/gidolinya.fnt"
  let font :: Font Int = fromRight (error "Can't parse font") $ makeFont text "gidolinya"

  pure $ Resources {..}

-- We need a dynamic buffer per frame to write text mesh data
loadPerFrameResources :: Size Int -> VulkanResources -> SwapchainContext -> Managed H.DynamicBufferedMesh
loadPerFrameResources _ vr _sc = H.withDynamicBufferedMesh vr 1000

-- Our render function
renderGame :: MonadIO m => Size Scalar -> Model -> NominalDiffTime -> (Resources, H.DynamicBufferedMesh, CommandBuffer) -> m ()
renderGame scrSize Model { playerPos, missiles } _gameTime (Resources {..}, textBuffer, commandBuffer) =
  targetCommandBuffer commandBuffer . useDynamicMesh textBuffer $ do
    H.runMatrixT . H.xform (gameCameraMatrix scrSize) $ do
      useMaterial circleMaterial do
        for_ missiles \(pos, _) -> H.xform (mkTranslation pos !*! mkScale (V2 5 5)) do
          mat :: M44 Scalar <- fmap (fmap realToFrac) . transpose <$> H.askMatrix
          pushConstant mat
          draw square

      useMaterial solidMaterial do
        H.xform (mkTranslation playerPos !*! mkScale (V2 10 10)) do
          mat :: M44 Scalar <- fmap (fmap realToFrac) . transpose <$> H.askMatrix
          pushConstant (SolidColorPushConstant mat (V4 1 0 0 1))
          draw square

    H.runMatrixT . H.xform (uiCameraMatrix scrSize) $ do
      H.xform (mkTranslation (topLeft 20 20 scrSize)) do
        useMaterial textMaterial do
          mat :: M44 Scalar <- fmap (fmap realToFrac) . transpose <$> H.askMatrix
          pushConstant mat
          drawText font (textcommand { color = white, text = "Arrow keys move, Space shoots", align = AlignLeft, fontSize = 5 } )

  where
  gameCameraMatrix size@(Size w _h) =
    let proj = Ortho w 1 100 True
        camera = Camera proj (V3 0 0 (-1)) (V3 0 0 1) (V3 0 (-1) 0)
    in viewProjectionMatrix camera (aspectRatio size)
  uiCameraMatrix size@(Size w _h) =
    let proj = Ortho w 1 100 False
        camera = Camera proj (V3 0 0 (-1)) (V3 0 0 1) (V3 0 (-1) 0)
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
buildNetwork :: CoreEventGenerators (Resources, H.DynamicBufferedMesh, CommandBuffer) -> IO ()
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

  GLFWV.runFrames win (loadResources "Example/Shooter/assets") loadPerFrameResources \resources textBuffer commandBuffer -> do
    coreEvProc (resources, textBuffer, commandBuffer)

    focused <- GLFW.getWindowFocused win
    -- don't consume CPU when the window isn't focused
    unless focused (threadDelay 100000)

vertShader :: B.ByteString
vertShader = [vert|
  #version 450

  layout(location = 0) in vec3 inPosition;

  layout( push_constant ) uniform constants
  {
    mat4 modelViewMatrix;
    vec4 color;
  } PushConstants;

  void main() {
      gl_Position = PushConstants.modelViewMatrix * vec4(inPosition, 1.0);
  }

|]

fragShader :: B.ByteString
fragShader = [frag|
  #version 450

  layout(location = 0) out vec4 outColor;

  layout( push_constant ) uniform constants
  {
    mat4 modelViewMatrix;
    vec4 color;
  } PushConstants;

  void main() {
    outColor = PushConstants.color;
  }

|]

texVertShader :: B.ByteString
texVertShader = [vert|
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

textVertShader :: B.ByteString
textVertShader = [vert|
  #version 450

  layout(location = 0) in vec3 inPosition;
  layout(location = 1) in vec4 inColor;
  layout(location = 3) in vec2 inTexCoord;

  layout(location = 0) out vec4 fragColor;
  layout(location = 1) out vec2 texCoord;

  layout( push_constant ) uniform constants
  {
    mat4 modelViewMatrix;
  } PushConstants;

  void main() {
      gl_Position = PushConstants.modelViewMatrix * vec4(inPosition, 1.0);
      texCoord = inTexCoord;
      fragColor = inColor;
  }

|]

textFragShader :: B.ByteString
textFragShader = [frag|
  #version 450

  layout(location = 0) in vec4 fragColor;
  layout(location = 1) in vec2 texCoord;

  layout(binding = 0) uniform sampler2D texSampler;

  layout(location = 0) out vec4 outColor;

  void main() {
    outColor = texture(texSampler, texCoord) * fragColor;
  }

|]
