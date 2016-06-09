{- shooter3.hs
 - This game has simple character movement with the arrow keys,
 - as well as shooting simple missiles with the SpaceBar.
 -}

{-# LANGUAGE NamedFieldPuns #-}

import Data.List
import Data.Maybe
import Engine.Scene.Input
import Math.Vector
import Math.Matrix
import Graphics.Drawing
import Graphics.DrawUtils
import Graphics.Shader
import Camera.Camera
import Textures.Textures
import Types.Types
import Types.Color
import Control.Monad
import qualified Graphics.UI.GLFW as GLFW
import Platforms.GLFW

import Layer.Layer

import Data.Bits
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB.GeometryShader4

-- Our game data
data Model = Model { 
           playerPos :: V2,
           playerMoveDir :: V2,
           firingDirection :: V2,
           missiles :: [(V2, V2)]
           }

-- By default, our firingDirection is to the right
newGame :: Model
newGame = Model vZero vZero (v2 1 0) []

data Msg = Fire | AddMove V2 | SubMove V2

uiLayer :: Double -> Layer ((), Model) RawInput
uiLayer input = constructStatelessLayer (\_ ri -> maybeToList (makeMsg ri)) (gameLayer input)

gameLayer :: Double -> Layer Model Msg
gameLayer delta mdl msgs = physics delta $ foldl' update mdl msgs

-- Our event type
type Event = RawInput

isMoveKey :: Key -> Bool
isMoveKey key = key `elem` [Key'Up, Key'Down, Key'Left, Key'Right]

moveKeyVec :: Key -> V2
moveKeyVec Key'Up = v2 0 1
moveKeyVec Key'Down = v2 0 (-1)
moveKeyVec Key'Left = v2 (-1) 0
moveKeyVec Key'Right = v2 1 0
moveKeyVec _ = vZero

makeMsg :: Event -> Maybe Msg
makeMsg (InputKeyDown Key'Space) = Just Fire
makeMsg (InputKeyDown key) | isMoveKey key = Just (AddMove (moveKeyVec key))
makeMsg (InputKeyUp key dur) | isMoveKey key = Just (SubMove (moveKeyVec key))
makeMsg _ = Nothing

playerMovementSpeed = 100
missileMovementSpeed = 200

missileInBounds :: (V2, V2) -> Bool
missileInBounds (pos, _) = vmag pos < 500

adjustMoveDir :: V2 -> Model -> Model
adjustMoveDir dir model@Model { playerMoveDir, firingDirection } = model { playerMoveDir = newMoveDir, firingDirection = if vnull newMoveDir then firingDirection else newMoveDir }
    where newMoveDir = playerMoveDir + dir

update :: Model -> Msg -> Model
update model@Model { playerPos, firingDirection, missiles } Fire = model { missiles = missiles' }
    where missiles' = (playerPos, firingDirection) : missiles
update model (AddMove dir) = adjustMoveDir dir model
update model (SubMove dir) = adjustMoveDir (- dir) model

physics :: Double -> Model -> Model
physics delta model@Model { playerPos, playerMoveDir, missiles } = model { playerPos = playerPos', missiles = missiles' }
    where missiles' = filter missileInBounds $ map (\(pos, dir) -> (pos + (dir |* (delta * missileMovementSpeed)), dir)) missiles
          playerPos' = playerPos + (playerMoveDir |* (delta * playerMovementSpeed))

-- The resources used by our rendering function
data Resources = Resources {
               solidShader :: Shader,
               texturedShader :: Shader,
               missileTex :: TexID
               }

loadResources :: String -> IO Resources
loadResources path = do
        solid <- loadShader path "Shader.vsh" "SolidColor.fsh"

        -- To draw the missiles, we also need a shader that can draw
        -- textures, and the actual missile texture
        textured <- loadShader path "Shader.vsh" "Shader.fsh"
        missiletex <- loadTexture path "circle.png"

        case (solid, textured, missiletex) of
            (Just solSh, Just texSh, Just missTex) -> return $ Resources solSh texSh missTex
            _ -> error "Couldn't load resources."

-- This function calculates a view matrix, used during rendering
calcCameraMatrix :: Size Int -> Model -> Mat44
calcCameraMatrix (Size w h) model = 
        let proj = Ortho (realToFrac w) 1 100 True
            camera = Camera proj vZero in
                cameraMatrix camera (aspectRatio (Size w h))

-- Our render function
render :: Resources -> RenderLayer -> Model -> IO ()
render Resources { solidShader, texturedShader, missileTex } layer  Model { playerPos, missiles } = do
        drawSpec (v2tov3 playerPos (-5)) layer (SolidSquare (Size 10 10) white solidShader)

        -- Draw the missiles
        forM_ missiles $ \(pos, _) ->
            drawSpec (v2tov3 pos (-5)) layer (Square (Size 5 5) (rgb 1 0 0) missileTex texturedShader)


gameMain :: GLFW.Window -> Size Int -> IO ()
gameMain win scrSize = do 
    initRenderer
    glClearColor 0.125 0.125 0.125 1
    glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA
    glActiveTexture gl_TEXTURE0

    glEnable gl_PROGRAM_POINT_SIZE -- for OSX

    resources <- loadResources "resources"
    inputPoller <- makeInputPoller win
    timePoller <- makeTimePoller

    let go mdl = do
        delta <- timePoller
        input <- inputPoller

        let mat = calcCameraMatrix scrSize mdl

        glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)

        render resources worldLayer mdl
        renderCommands mat worldLayer

        resetRenderer
        GLFW.swapBuffers win

        let (_, mdl') = uiLayer delta ((), mdl) input

        go mdl'

    go newGame

main :: IO ()
main = glfwMain' "Demo"
                  (Size 480 640)
                  gameMain
