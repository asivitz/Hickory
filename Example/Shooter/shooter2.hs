import Engine.Scene.Scene
import Engine.Scene.Input
import Types.Types
import Math.Vector
import Math.Matrix
import Graphics.Drawing
import GLFW.Run
import Camera.Camera
import Systems.Draw

-- Our game data
data Model = Model { 
           playerPos :: V2
           }

newGame :: Model
newGame = Model pZero

-- Our event type
type Event = RawInput

-- We need to collect our events into an input data structure that makes
-- sense for our game
data GameInput = GameInput (Maybe V2)

collectInput events = foldl process (GameInput Nothing) events
    where process gameInput (InputKeysHeld hash) = 
            let movementVec = foldl (\vec (key,heldTime) -> vec + (case key of
                                                                        Key'W -> v2 0 1
                                                                        Key'A -> v2 1 0
                                                                        Key'S -> v2 0 (-1)
                                                                        Key'D -> v2 1 0))
                                    pZero
                                    (HashMap.toList hash)
                in gameInput (Just movementVec)
          process gameInput _ = gameInput

-- Our actual game logic simply turns the movementVector into actual player
-- movement
stepModel :: RenderInfo -> [Event] -> Double -> Model -> (Model, [Event])
stepModel renderinfo events delta model@Model { playerPos } =
        let (GameInput movementVec) = collectInput events 
            model' = case movementVec of
                         Nothing -> model
                         Just v -> model { playerPos + (v |* delta) }
                in (model', [])

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
            camera = Camera proj pZero in
                cameraMatrix camera (aspectRatio (Size w h))

-- Our render function
render :: Resources -> RenderInfo -> Model -> IO ()
render Resources { solidShader } (RenderInfo _ _ label)  Model { playerPos } = do
        drawSpec (v2tov3 playerPos (-5)) label (Square (Size 10 10) white nullTex solidShader)

makeScene :: String -> IO (SceneOperator (Event))
makeScene resPath = makeSceneOperator newGame
                                      stepModel
                                      (loadResources resPath)
                                      calcCameraMatrix
                                      render
                                      worldLabel

main :: IO ()
main = do
        operator <- makeScene "resources"
         
        glfwMain (Size 480 640)
            [operator]
            operator
            id
