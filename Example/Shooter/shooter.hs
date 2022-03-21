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
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.IO.Class (MonadIO)
import Data.IORef (IORef, newIORef)
import Data.Foldable (for_)
import Hickory.Camera
import Hickory.Color
import Hickory.FRP.Combinators (unionFirst)
import Hickory.FRP.CoreEvents (mkCoreEvents, CoreEvents(..), CoreEventGenerators)
import qualified Hickory.Graphics as H
import Hickory.Input
import Hickory.Math (Mat44, vnull, mkTranslation, mkScale)
import Hickory.Resources (pull, readerFromIORef)
import Linear (zero, V2(..), V3(..), (^*), (!*!))
import Hickory.Types
import Linear.Metric
import Platforms.GLFW.FRP (glfwCoreEventGenerators)
import Platforms.GLFW.Utils
import Reactive.Banana ((<@))
import qualified Graphics.UI.GLFW as GLFW
import qualified Reactive.Banana as B
import qualified Reactive.Banana.Frameworks as B
import qualified Data.HashMap.Strict as Map
import Data.Time.Clock (NominalDiffTime)

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
data Msg = Fire | AddMove Vec | SubMove Vec | Tick NominalDiffTime | Noop

-- By default, our firingDirection is to the right
newGame :: Model
newGame = Model zero zero (V2 1 0) []

-- Change the game model by processing some input
gameStep :: Msg -> Model -> Model
gameStep msg model@Model { playerPos, firingDirection, missiles } = case msg of
  Tick time -> physics time model
  Fire -> model { missiles = (playerPos, firingDirection) : missiles }
  AddMove dir -> adjustMoveDir dir model
  SubMove dir -> adjustMoveDir (- dir) model
  Noop -> model

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
  { missileTex     :: H.TexID
  , vaoCache       :: H.VAOCache
  }

-- Set up our scene, load assets, etc.
loadResources :: String -> IO Resources
loadResources path = do
  let shaderVersion = "410"
  -- To draw the missiles, we also need a shader that can draw
  -- textures, and the actual missile texture
  solid    <- H.loadSolidShader shaderVersion
  textured <- H.loadTexturedShader shaderVersion
  missiletex <- H.loadTexture' path ("circle.png", H.texLoadDefaults)

  -- We'll use some square geometry and draw our texture on top
  vaoCache <- Map.fromList <$> traverse sequence
    [("square", H.mkSquareVAOObj 0.5 solid)
    ,("squaretex", H.mkSquareVAOObj 0.5 textured)]

  pure $ Resources missiletex vaoCache

-- This function calculates a view matrix, used during rendering
calcCameraMatrix :: Size Int -> Mat44
calcCameraMatrix size@(Size w _h) =
  let proj = Ortho (realToFrac w) 1 100 True
      camera = Camera proj (V3 0 0 10) zero (V3 0 1 0)
  in viewProjectionMatrix camera (aspectRatio size)

-- Our render function
renderGame :: (MonadIO m, MonadReader Resources m) => Size Int -> Model -> NominalDiffTime -> m ()
renderGame scrSize Model { playerPos, missiles } _gameTime = do
  H.runMatrixT . H.xform (calcCameraMatrix scrSize) $ do
    missileTex <- asks missileTex
    tex <- pull vaoCache "squaretex"
    for_ missiles \(pos, _) -> H.xform (mkTranslation pos !*! mkScale (V2 5 5)) do
      H.drawVAO tex do
        H.bindTextures [missileTex]
        H.bindUniform "color" red
        H.bindMatrix "modelMat"
    pull vaoCache "square" >>= flip H.drawVAO do
      H.xform (mkTranslation playerPos !*! mkScale (V2 10 10)) $ H.bindMatrix "modelMat"
      H.bindUniform "color" white

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

procKeyDown :: Key -> Msg
procKeyDown k =
  if isMoveKey k
  then AddMove (moveKeyVec k)
  else case k of
    Key'Space -> Fire
    _ -> Noop

procKeyUp :: Key -> Msg
procKeyUp k =
  if isMoveKey k
  then SubMove (moveKeyVec k)
  else Noop

-- Build the FRP network
buildNetwork :: IORef Resources -> CoreEventGenerators -> IO ()
buildNetwork resRef evGens = do
  B.actuate <=< B.compile $ mdo
    coreEvents <- mkCoreEvents evGens

    -- currentTime isn't currently used, but it's useful for things like animation
    currentTime <- B.accumB 0 ((+) <$> eTime coreEvents)

    let evs = unionFirst [ Tick        <$> eTime coreEvents -- step the physics
                         , procKeyDown <$> keyDown coreEvents
                         , procKeyUp   <$> keyUp coreEvents
                         ]

    -- Step the game model forward every time we get a new event
    mdl <- B.accumB newGame (gameStep <$> evs)

    -- every time we get a 'render' event tick, draw the screen
    B.reactimate $ readerFromIORef resRef <$>
      (renderGame <$> scrSizeB coreEvents <*> mdl <*> currentTime)
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
