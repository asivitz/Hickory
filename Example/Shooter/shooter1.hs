import Engine.Scene.Scene
import Engine.Scene.Input
import Types.Types
import Math.Matrix
import Graphics.Drawing
import GLFW.Run

-- This file just contains stub functions and datatypes.
-- We'll fill them in as we build our example game.

-- Our game data
data Model = EmptyModel

-- Our event type
type Event = RawInput Int

-- This function runs our game logic. It steps through one frame of simulation.
-- This stub function does nothing to the model, and it returns an empty
-- array which means it does not add any events to the event stream.
stepModel :: RenderInfo -> [Event] -> Double -> Model -> (Model, [Event])
stepModel renderinfo inputEvents delta model =
        (model, [])

-- The resources used by our rendering function
data Resources = EmptyResources

-- This function loads and returns our resources
loadResources :: IO (Resources)
loadResources = return EmptyResources

-- This function calculates a view matrix, used during rendering
calcCameraMatrix :: Size Int -> Model -> Mat44
calcCameraMatrix (Size w h) model = mat44Identity

-- Our render function
render :: Resources -> RenderInfo -> Model -> IO ()
render resources renderinfo model = return ()


makeScene :: IO (SceneOperator (Event))
makeScene = makeSceneOperator EmptyModel
                stepModel
                loadResources
                calcCameraMatrix
                render
                worldLabel

main :: IO ()
main = do
        operator <- makeScene
         
        glfwMain (Size 480 640)
            [operator]
            operator
            id
