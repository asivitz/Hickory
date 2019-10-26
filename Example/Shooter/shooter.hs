{- shooter.hs
 - This game has simple character movement with the arrow keys,
 - as well as shooting simple missiles with the SpaceBar.
 -}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}



import Control.Concurrent (threadDelay)
import Control.Lens (Lens')
import Control.Monad (forever, when)
import Data.Foldable
import Data.Generics.Labels
import Data.Generics.Product
import Data.IORef (IORef, modifyIORef', newIORef)
import Data.Text (Text)
import Debug.Trace (traceShow)
import GHC.Generics (Generic)
import Hickory.Camera
import Hickory.Color
import Hickory.FRP (unionFirst, accumEvents, foldEventEmitter, render, mkCoreEvents, CoreEvents(..), CoreEventGenerators)
import Hickory.Graphics.DrawUtils (RenderTree(..), size3PosMat, colorUniform, DrawSpec(..), VAOObj(..), sizePosMat, mkSquareVAOObj, HasRenderState(..), RenderState(..))
import Hickory.Graphics.Drawing
import Hickory.Graphics.GLSupport
import Hickory.Graphics.Shader
import Hickory.Graphics.StockShaders (loadSolidShader, loadSkinnedShader, loadTexturedShader, loadPVCShader)
import Hickory.Graphics.Textures
import Hickory.Input
import Hickory.Math.Matrix (Mat44)
import Hickory.Math.Vector
import Hickory.Types
import Linear.Metric
import Platforms.GLFW
import Platforms.GLFW.FRP (glfwCoreEventGenerators)
import Platforms.GLFW.Utils
import Reactive.Banana ((<@>), (<@))
import System.Mem (performMinorGC)
import qualified Graphics.UI.GLFW as GLFW
import qualified Reactive.Banana as B
import qualified Reactive.Banana.Frameworks as B

-- ** GAMEPLAY **

type Vec = V2 Double

-- Our game data
data Model = Model
  { playerPos :: Vec
  , playerMoveDir :: Vec
  , firingDirection :: Vec
  , missiles :: [(Vec, Vec)]
  }

-- All the possible inputs
data Msg = Fire | AddMove Vec | SubMove Vec | Tick Double | Noop

-- By default, our firingDirection is to the right
newGame :: Model
newGame = Model zero zero (v2 1 0) []

-- Change the game model by processing some input
gameStep :: Msg -> Model -> Model
gameStep msg model@Model { playerPos, firingDirection, missiles } = case msg of
    Tick time -> physics time model
    Fire -> model { missiles = (playerPos, firingDirection) : missiles }
    AddMove dir -> adjustMoveDir dir model
    SubMove dir -> adjustMoveDir (- dir) model
    Noop -> model

-- Step the world forward by some small delta
physics :: Double -> Model -> Model
physics delta model@Model { playerPos, playerMoveDir, missiles } = model
  { playerPos = playerPos + (playerMoveDir ^* (delta * playerMovementSpeed))
  , missiles = filter missileInBounds $
      map (\(pos, dir) -> (pos + (dir ^* (delta * missileMovementSpeed)), dir)) missiles
  }

-- Some gameplay constants
playerMovementSpeed = 100
missileMovementSpeed = 200

-- Pure utilities
missileInBounds :: (Vec, Vec) -> Bool
missileInBounds (pos, _) = norm pos < 500

adjustMoveDir :: Vec -> Model -> Model
adjustMoveDir dir model@Model { playerMoveDir, firingDirection } = model { playerMoveDir = newMoveDir, firingDirection = if vnull newMoveDir then firingDirection else newMoveDir }
    where newMoveDir = playerMoveDir + dir

-- ** RENDERING **

-- The resources used by our rendering function
data Resources = Resources
  { solidShader :: Shader
  , texturedShader :: Shader
  , missileTex :: TexID
  , squareVAOObj :: VAOObj
  , resourceRenderState :: RenderState
  } deriving (Generic)

instance HasRenderState Resources where
  renderState = #resourceRenderState :: Lens' Resources RenderState

-- Set up our scene, load assets, etc.
initRenderState :: Text -> String -> IO Resources
initRenderState shaderVersion path = do
  configGLState 0.125 0.125 0.125
  solid <- loadSolidShader shaderVersion
  -- To draw the missiles, we also need a shader that can draw
  -- textures, and the actual missile texture
  textured <- loadTexturedShader shaderVersion
  missiletex <- loadTexture' path ("circle.png", texClamp)

  squareVAOObj <- mkSquareVAOObj textured

  let rs = RenderState (zip (repeat Nothing) [])

  pure $ Resources solid textured missiletex squareVAOObj rs

-- This function calculates a view matrix, used during rendering
calcCameraMatrix :: Size Int -> Mat44
calcCameraMatrix size@(Size w h) =
  let proj = Ortho (realToFrac w) 1 100 True
      camera = Camera proj (V3 0 0 10) zero (V3 0 1 0)
  in cameraMatrix camera (aspectRatio size)

-- Our render function
renderGame :: Size Int -> Model -> Scalar -> Mat44 -> Resources -> RenderTree
renderGame _scrSize Model { playerPos, missiles } _gameTime mat Resources { solidShader, texturedShader, missileTex, squareVAOObj }  =
  XForm (Just mat) $ List [playerRT, missilesRT]
  where
    playerRT = Primative solidShader (sizePosMat (Size 10 10) (v2tov3 playerPos 0)) [colorUniform white] (VAO Nothing squareVAOObj)
    missilesRT = List $ map (\(pos, _) -> Primative texturedShader (sizePosMat (Size 5 5) (v2tov3 pos 0)) [colorUniform $ rgb 1 0 0] (VAO (Just missileTex) squareVAOObj)) missiles

-- ** INPUT **

-- Translating raw input to game input
isMoveKey :: Key -> Bool
isMoveKey key = key `elem` [Key'Up, Key'Down, Key'Left, Key'Right]

moveKeyVec :: Key -> Vec
moveKeyVec key = case key of
    Key'Up -> v2 0 1
    Key'Down -> v2 0 (-1)
    Key'Left -> v2 (-1) 0
    Key'Right -> v2 1 0
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
  (B.actuate =<<) . B.compile $ mdo
    coreEvents    <- mkCoreEvents evGens

    -- currentTime isn't currently used, but it's useful for things like animation
    currentTime <- B.accumB 0 ((+) <$> eTime coreEvents)

    let evs = unionFirst [ Tick        <$> eTime coreEvents -- step the physics
                         , procKeyDown <$> keyDown coreEvents
                         , procKeyUp   <$> keyUp coreEvents
                         ]

    -- Step the game model forward every time we get a new event
    mdl <- B.accumB newGame (gameStep <$> evs)

    -- The camera matrix is dynamically based on the current screen size
    let mat = calcCameraMatrix <$> scrSizeB coreEvents

    -- every time we get a 'render' event tick, draw the screen
    B.reactimate $ render resRef
      [ renderGame <$> scrSizeB coreEvents <*> mdl <*> currentTime <*> mat
      ] <@ eRender coreEvents

main :: IO ()
main = withWindow 750 750 "Demo" $ \win -> do
  resources <- newIORef
           =<< initRenderState "410" "assets"

  -- setup event generators for core input (keys, mouse clicks, and elapsed time, etc.)
  (coreEvProc, evGens) <- glfwCoreEventGenerators win

  -- build and run the FRP network
  buildNetwork resources evGens

  forever $ do
    coreEvProc -- check the input buffers and generate FRP events

    GLFW.swapBuffers win -- present latest drawn frame

    focused <- GLFW.getWindowFocused win

    -- don't consume CPU when the window isn't focused
    when (not focused) (traceShow "Window Defocused" $ threadDelay 100000)

    performMinorGC -- try to smooth out the GC a bit
