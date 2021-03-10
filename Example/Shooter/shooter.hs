{- shooter.hs
 - This game has simple character movement with the arrow keys,
 - as well as shooting simple missiles with the SpaceBar.
 -}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}



import Control.Concurrent (threadDelay)
import Control.Monad (forever, unless, (<=<))
import Data.IORef (IORef, newIORef)
import Data.Text (Text)
import Data.Functor ((<&>))
import Hickory.Camera
import Hickory.Color
import Hickory.FRP (unionFirst, mkCoreEvents, CoreEvents(..), CoreEventGenerators, renderWithRef)
import qualified Hickory.Graphics as H
import Hickory.Input
import Hickory.Math (Mat44, vnull, Scalar, mkTranslation, mkScale)
import Linear (zero, V2(..), V3(..), (^*), (!*!))
import Hickory.Types
import Linear.Metric
import Platforms.GLFW.FRP (glfwCoreEventGenerators)
import Platforms.GLFW.Utils
import Reactive.Banana ((<@))
import qualified Graphics.UI.GLFW as GLFW
import qualified Reactive.Banana as B
import qualified Reactive.Banana.Frameworks as B
import qualified Data.Map as Map

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
data Msg = Fire | AddMove Vec | SubMove Vec | Tick Double | Noop

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
physics :: Double -> Model -> Model
physics delta model@Model { playerPos, playerMoveDir, missiles } = model
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
initRenderState :: Text -> String -> IO Resources
initRenderState shaderVersion path = do
  H.configGLState 0.125 0.125 0.125
  -- To draw the missiles, we also need a shader that can draw
  -- textures, and the actual missile texture
  textured <- H.loadTexturedShader shaderVersion
  missiletex <- H.loadTexture' path ("circle.png", H.texClamp)

  -- We'll use some square geometry and draw our texture on top
  squareVAOObj <- H.mkSquareVAOObj textured
  let vaoCache = Map.singleton "square" squareVAOObj

  pure $ Resources missiletex vaoCache

-- This function calculates a view matrix, used during rendering
calcCameraMatrix :: Size Int -> Mat44
calcCameraMatrix size@(Size w _h) =
  let proj = Ortho (realToFrac w) 1 100 True
      camera = Camera proj (V3 0 0 10) zero (V3 0 1 0)
  in cameraMatrix camera (aspectRatio size)

-- Our render function
renderGame :: Size Int -> Model -> Scalar -> Mat44 -> Resources -> H.RenderTree
renderGame _scrSize Model { playerPos, missiles } _gameTime mat Resources { missileTex, vaoCache }  =
  H.XForm mat $ H.List [playerRT, missilesRT]
  where
    playerRT = H.XForm (mkTranslation playerPos !*! mkScale (V2 10 10)) . pullVaoCache "square" $
      H.Primitive [H.colorUniform white] Nothing
    missilesRT = pullVaoCache "square" \vo ->
      H.List $ missiles <&> \(pos, _) -> H.XForm (mkTranslation pos !*! mkScale (V2 5 5)) $
        H.Primitive
          [H.colorUniform red]
          (Just missileTex)
          vo
    pullVaoCache k f = maybe H.NoRender (f . H.VAO) $ Map.lookup k vaoCache

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
    B.reactimate $ renderWithRef resRef
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
    unless focused (threadDelay 100000)
