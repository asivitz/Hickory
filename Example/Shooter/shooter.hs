{- shooter3.hs
 - This game has simple character movement with the arrow keys,
 - as well as shooting simple missiles with the SpaceBar.
 -}

{-# LANGUAGE NamedFieldPuns #-}

import Hickory.Input
import Hickory.Math.Vector
import Linear.Metric
import Hickory.Math.Matrix
import Hickory.Graphics.Drawing
import Hickory.Graphics.DrawUtils
import Hickory.Graphics.Shader
import Hickory.Camera
import Hickory.Graphics.Textures
import Hickory.Types
import Hickory.Color
import qualified Graphics.UI.GLFW as GLFW
import Platforms.GLFW
import Platforms.GLFW.Utils

import Hickory.Graphics.GLSupport

import Control.Layer
import Hickory.Utils.Layer
import Data.Foldable

type Vec = V2 Double

-- Our game data
data Model = Model {
           playerPos :: Vec,
           playerMoveDir :: Vec,
           firingDirection :: Vec,
           missiles :: [(Vec, Vec)]
           }

-- By default, our firingDirection is to the right
newGame :: Model
newGame = Model zero zero (v2 1 0) []

data Msg = Fire | AddMove Vec | SubMove Vec | Tick Double

gameLayer :: Layer Model Msg
gameLayer model@Model { playerPos, firingDirection, missiles } msg = case msg of
    Tick time -> physics time model
    Fire -> model { missiles = (playerPos, firingDirection) : missiles }
    AddMove dir -> adjustMoveDir dir model
    SubMove dir -> adjustMoveDir (- dir) model

playerMovementSpeed = 100
missileMovementSpeed = 200

missileInBounds :: (Vec, Vec) -> Bool
missileInBounds (pos, _) = norm pos < 500

adjustMoveDir :: Vec -> Model -> Model
adjustMoveDir dir model@Model { playerMoveDir, firingDirection } = model { playerMoveDir = newMoveDir, firingDirection = if vnull newMoveDir then firingDirection else newMoveDir }
    where newMoveDir = playerMoveDir + dir

physics :: Double -> Model -> Model
physics delta model@Model { playerPos, playerMoveDir, missiles } = model {
        playerPos = playerPos + (playerMoveDir ^* (delta * playerMovementSpeed)),
        missiles = filter missileInBounds $
                        map (\(pos, dir) -> (pos + (dir ^* (delta * missileMovementSpeed)), dir)) missiles
    }

-- The resources used by our rendering function
data Resources = Resources {
               solidShader :: Shader,
               texturedShader :: Shader,
               missileTex :: TexID,
               squareVAOObj :: VAOObj
               }

loadResources :: String -> IO Resources
loadResources path = do
        solid <- loadShader path "Shader.vsh" "SolidColor.fsh"
        -- To draw the missiles, we also need a shader that can draw
        -- textures, and the actual missile texture
        textured <- loadShader path "Shader.vsh" "Shader.fsh"
        missiletex <- loadTexture' path "circle.png"

        squareVAOObj <- mkSquareVAOObj textured

        return $ Resources solid textured missiletex squareVAOObj

-- This function calculates a view matrix, used during rendering
calcCameraMatrix :: Size Int -> Mat44
calcCameraMatrix size@(Size w h) =
        let proj = Ortho (realToFrac w) 1 100 True
            camera = Camera proj (V3 0 0 10) zero in
                cameraMatrix camera (aspectRatio size)

-- Our render function
renderGame :: Resources -> Model -> RenderTree
renderGame Resources { solidShader, texturedShader, missileTex, squareVAOObj } Model { playerPos, missiles } = List [playerRT, missilesRT]
    where playerRT = Primative solidShader (sizePosMat (Size 10 10) (v2tov3 playerPos 0)) white (VAO Nothing squareVAOObj)
          missilesRT = List $ map (\(pos, _) -> Primative texturedShader (sizePosMat (Size 5 5) (v2tov3 pos 0)) (rgb 1 0 0) (VAO (Just missileTex) squareVAOObj)) missiles

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

makeMsg :: RawInput -> [Msg]
makeMsg input = case input of
    (InputKeyDown Key'Space) -> [Fire]
    (InputKeyDown key) | isMoveKey key -> [AddMove (moveKeyVec key)]
    (InputKeyUp key dur) | isMoveKey key -> [SubMove (moveKeyVec key)]
    (Step scal) -> [Tick scal]
    _ -> []

type AppState = (Resources, Model)

renderLayer :: Size Int -> MonadicLayer IO AppState RawInput
renderLayer scrSize = constructMonadicLayer doren runGameLayer
    where mat = calcCameraMatrix scrSize
          runGameLayer = liftInput makeMsg gameLayer
          doren :: Model -> Resources -> IO Resources
          doren model resources = do
              clearScreen
              _ <- renderTree mat (renderGame resources model) (RenderState [])
              return resources

newAppState :: String -> IO AppState
newAppState path = do
        res <- loadResources path
        return (res, newGame)

main :: IO ()
main = withWindow 480 640 "Demo" $ \win -> do
    configGLState 0.125 0.125 0.125

    initState <- newAppState "resources"

    inputPoller <- makeGLFWInputPoller win

    let loop appstate = do
            input <- inputPoller
            scrSize <- uncurry Size <$> GLFW.getFramebufferSize win

            appstate' <- foldlM (renderLayer scrSize) appstate input

            GLFW.swapBuffers win

            loop appstate'

    loop initState
