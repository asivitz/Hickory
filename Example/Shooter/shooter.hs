{- shooter.hs
 - This game has simple character movement with the arrow keys,
 - as well as shooting simple missiles with the SpaceBar.
 -}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Concurrent (threadDelay)
import Control.Monad (forever, unless, (<=<))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef)
import Data.Maybe (maybeToList, fromJust)
import Data.Time.Clock (NominalDiffTime)
import Hickory.Camera
import Hickory.Color
import Hickory.FRP.CoreEvents (mkCoreEvents, CoreEvents(..), CoreEventGenerators)
import Hickory.FRP.Game (timeStep)
import Hickory.FRP.UI (topLeft)
import Hickory.FRP.Historical (historical)
import Hickory.Input
import Hickory.Math (vnull, mkTranslation, mkScale)
import Hickory.Resources (pull, readerFromIORef)
import Hickory.Types
import Linear (zero, V2(..), V3(..), (^*), (!*!))
import Linear.Metric
import Platforms.GLFW.FRP (glfwCoreEventGenerators)
import Platforms.GLFW.Utils
import Reactive.Banana ((<@))
import qualified Data.HashMap.Strict as Map
import qualified Graphics.UI.GLFW as GLFW
import qualified Hickory.Graphics as H
import qualified Hickory.Text.Text as H
import qualified Reactive.Banana as B
import qualified Reactive.Banana.Frameworks as B

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
newGame = Model zero zero (V2 1 0) []

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
  { missileTex  :: H.TexID
  , vaoCache    :: H.VAOCache -- Holds geometry, e.g. simple squares or complex 3d models
  , printer     :: H.Printer Int -- For text rendering
  }

-- Set up our scene, load assets, etc.
loadResources :: String -> IO Resources
loadResources path = do
  let shaderVersion = "410"
  -- To draw the missiles, we also need a shader that can draw
  -- textures, and the actual missile texture
  solid    <- H.loadSolidShader shaderVersion
  textured <- H.loadTexturedShader shaderVersion

  -- A shader for drawing text
  pvcshader <- H.loadPVCShader shaderVersion

  -- Our missile texture
  missiletex <- H.loadTexture' path ("circle.png", H.texLoadDefaults)

  -- gidolinya.fnt (font data) and gidolinya.png (font texture) were
  -- generated using the bmGlyph program for Mac
  pr <- H.loadPrinter path pvcshader "gidolinya"

  -- We'll use some square geometry and draw our texture on top
  vaoCache <- Map.fromList <$> traverse sequence
    [("square",    H.mkSquareVAOObj 0.5 solid)     -- for solid color squares
    ,("squaretex", H.mkSquareVAOObj 0.5 textured)] -- for textured squares

  pure $ Resources missiletex vaoCache (fromJust pr)

-- Our render function
renderGame :: (MonadIO m, MonadReader Resources m, H.DynamicVAOMonad m, H.PrinterMonad m) => Size Double-> Model -> NominalDiffTime -> m ()
renderGame scrSize Model { playerPos, missiles } _gameTime = do
  H.runMatrixT . H.xform (gameCameraMatrix scrSize) $ do
    missileTex <- asks missileTex
    tex <- pull vaoCache "squaretex"
    for_ missiles \(pos, _) -> H.xform (mkTranslation pos !*! mkScale (V2 5 5)) do
      H.drawVAO tex do -- within a draw command, we can bind inputs to our shader
        H.bindTextures [missileTex]
        H.bindUniform "color" red
        H.bindMatrix "modelMat"
    pull vaoCache "square" >>= flip H.drawVAO do
      H.xform (mkTranslation playerPos !*! mkScale (V2 10 10)) $ H.bindMatrix "modelMat"
      H.bindUniform "color" white

  H.runMatrixT . H.xform (uiCameraMatrix scrSize) $ do
    H.xform (mkTranslation (topLeft 20 20 scrSize)) do
      H.drawText $ H.textcommand { H.color = white, H.text = "Arrow keys move, Space shoots", H.align = H.AlignLeft, H.fontSize = 5 }

  where
  gameCameraMatrix size@(Size w _h) =
    let proj = Ortho w 1 100 True
        camera = Camera proj (V3 0 0 10) zero (V3 0 1 0)
    in viewProjectionMatrix camera (aspectRatio size)
  uiCameraMatrix size@(Size w _h) =
    let proj = Ortho w (-20) 1 False
        camera = Camera proj zero (V3 0 0 (-1)) (V3 0 1 0)
    in viewProjectionMatrix camera (aspectRatio size)

-- ** INPUT **

-- Translating raw input to game input
isMoveKey :: Key -> Bool
isMoveKey key = key `elem` [Key'Up, Key'Down, Key'Left, Key'Right]

moveKeyVec :: Key -> Vec
moveKeyVec key = case key of
  Key'Up    -> V2 0 1
  Key'Down  -> V2 0 (-1)
  Key'Left  -> V2 (-1) 0
  Key'Right -> V2 1 0
  _ -> zero

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
buildNetwork :: IORef Resources -> CoreEventGenerators -> IO ()
buildNetwork resRef evGens = do
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
    B.reactimate $ readerFromIORef resRef . H.withDynamicVAOs . H.withPrinting printer <$>
      (renderGame <$> (fmap realToFrac <$> scrSizeB coreEvents) <*> mdl <*> currentTime)
      <@ eRender coreEvents

main :: IO ()
main = withWindow 750 750 "Demo" $ \win -> do
  H.configGLState 0.125 0.125 0.125
  resources <- newIORef
           =<< loadResources "Example/Shooter/assets"

  -- setup event generators for core input (keys, mouse clicks, and elapsed time, etc.)
  (coreEvProc, evGens) <- glfwCoreEventGenerators win

  -- build and run the FRP network
  buildNetwork resources evGens

  forever $ do
    coreEvProc -- check the input buffers and generate FRP events

    GLFW.swapBuffers win -- present latest drawn frame
    H.clearDepth
    H.clearScreen

    focused <- GLFW.getWindowFocused win

    -- don't consume CPU when the window isn't focused
    unless focused (threadDelay 100000)
