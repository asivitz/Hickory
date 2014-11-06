{- shooter2.hs
 - This game has simple character movement, with the arrow keys.
 - See shooter3.hs to see a more developed example.
 -}

import Engine.Scene.Scene
import Engine.Scene.Input
import Types.Types
import Math.Matrix
import Graphics.Drawing
import Platforms.GLFW

import Math.Vector
import Graphics.DrawUtils
import Graphics.Shader
import Camera.Camera
import Types.Color
import qualified Data.HashMap.Strict as HashMap

-- Our game data
data Model = Model { 
           playerPos :: V2
           }

newGame :: Model
newGame = Model vZero

-- Our event type
type Event = RawInput

-- We need to collect our events into an input data structure that makes
-- sense for our game
data GameInput = GameInput (Maybe V2)

collectInput :: [Event] -> GameInput
collectInput events = foldl process (GameInput Nothing) events
    where process gameInput (InputKeysHeld hash) = 
            let movementVec = foldl (\vec (key,heldTime) -> vec + (case key of
                                                                        Key'Up -> v2 0 1
                                                                        Key'Left -> v2 (-1) 0
                                                                        Key'Down -> v2 0 (-1)
                                                                        Key'Right -> v2 1 0
                                                                        _ -> vZero))
                                    vZero
                                    (HashMap.toList hash)
                in GameInput (Just movementVec)
          process gameInput _ = gameInput

playerMovementSpeed = 100

-- Our actual game logic simply turns the movementVector into actual player
-- movement
stepModel :: RenderInfo -> [Event] -> Double -> Model -> (Model, [Event])
stepModel renderinfo events delta model@Model { playerPos = p } =
        let (GameInput movementVec) = collectInput events 
            newPlayerPos = case movementVec of
                         Nothing -> p
                         Just v -> p + (v |* (delta * playerMovementSpeed))
                in (model { playerPos = newPlayerPos }, [])

-- The resources used by our rendering function
data Resources = Resources {
               solidShader :: Shader
               }

-- We load a simple shader for drawing the player
loadResources :: String -> IO Resources
loadResources path = do
        solid <- loadShader path "Shader.vsh" "SolidColor.fsh"
        case solid of
            Just sh -> return $ Resources sh
            Nothing -> error "Couldn't load resources."

-- This function calculates a view matrix, used during rendering
calcCameraMatrix :: Size Int -> Model -> Mat44
calcCameraMatrix (Size w h) model = 
        let proj = Ortho (realToFrac w) 1 100 True
            camera = Camera proj vZero in
                cameraMatrix camera (aspectRatio (Size w h))

-- Our render function
render :: Resources -> RenderInfo -> Model -> IO ()
render Resources { solidShader = solid } (RenderInfo _ _ layer)  Model { playerPos = p } = do
        drawSpec (v2tov3 p (-5)) layer (SolidSquare (Size 10 10) white solid)

makeScene :: String -> IO (SceneOperator Event)
makeScene resPath = makeSceneOperator newGame
                                      stepModel
                                      (loadResources resPath)
                                      calcCameraMatrix
                                      render
                                      worldLayer

main :: IO ()
main = do
        operator <- makeScene "resources"
         
        glfwMain "Demo"
                 (Size 480 640)
                 [operator]
                 (_addEvent operator)
