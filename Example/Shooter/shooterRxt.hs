{- shooter3.hs
 - This game has simple character movement with the arrow keys,
 - as well as shooting simple missiles with the SpaceBar.
 -}

{-# LANGUAGE NamedFieldPuns #-}

import Debug.Trace

import Data.List
import Data.Maybe
import Engine.Scene.Scene
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
import Data.IORef
import qualified Graphics.UI.GLFW as GLFW
import qualified Platforms.GLFW.Bridge as Bridge
import qualified Data.HashMap.Strict as HashMap

import React.React

import Data.Time
import Data.Bits
import Platforms.GLFW.Utils
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB.GeometryShader4

-- Our game data
data Model = Model { 
           playerPos :: V2,
           playerMoveDir :: V2,
           firingDirection :: V2,
           missiles :: [(V2, V2)]
           }

-- By default, or firingDirection is to the right
newGame :: Model
newGame = Model vZero vZero (v2 1 0) []

data Msg = Fire | AddMove V2 | SubMove V2

-- Our event type
type Event = RawInput

isMoveKey key = elem key [Key'Up, Key'Down, Key'Left, Key'Right]

moveKeyVec Key'Up = v2 0 1
moveKeyVec Key'Down = v2 0 (-1)
moveKeyVec Key'Left = v2 (-1) 0
moveKeyVec Key'Right = v2 1 0

makeMsg :: Event -> Maybe Msg
makeMsg (InputKeyDown Key'Space) = Just Fire
makeMsg (InputKeyDown key) | isMoveKey key = Just (AddMove (moveKeyVec key))
makeMsg (InputKeyUp key dur) | isMoveKey key = Just (SubMove (moveKeyVec key))
makeMsg _ = Nothing

playerMovementSpeed = 100
missileMovementSpeed = 200

missileInBounds :: (V2, V2) -> Bool
missileInBounds (pos, _) = vmag pos < 500

adjustMoveDir dir model@Model { playerMoveDir, firingDirection } = model { playerMoveDir = newMoveDir, firingDirection = if vnull newMoveDir then firingDirection else newMoveDir }
    where newMoveDir = playerMoveDir + dir

update :: Msg -> Model -> Model
update Fire model@Model { playerPos, firingDirection, missiles } = model { missiles = missiles' }
    where missiles' = (playerPos, firingDirection) : missiles
update (AddMove dir) model = adjustMoveDir dir model
update (SubMove dir) model = adjustMoveDir (- dir) model

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
render :: Resources -> RenderInfo -> Model -> IO ()
render Resources { solidShader, texturedShader, missileTex } (RenderInfo _ _ layer)  Model { playerPos, missiles } = do
        drawSpec (v2tov3 playerPos (-5)) layer (SolidSquare (Size 10 10) white solidShader)

        -- Draw the missiles
        forM_ missiles $ \(pos, _) ->
            drawSpec (v2tov3 pos (-5)) layer (Square (Size 5 5) (rgb 1 0 0) missileTex texturedShader)

makeInputPoller :: GLFW.Window -> IO (IO [Event])
makeInputPoller win = do
        is <- newIORef []
        stepInp <- Bridge.setupInput win (addRawInput is)
        return $ do
            stepInp
            atomicModifyIORef is (\i -> ([], i))
    where addRawInput stream event = do
            atomicModifyIORef stream (\evs -> ((evs ++ [event]), ()))

doglfw :: String -> Size Int -> IO ()
doglfw name (Size w h) = do 
    withWindow w h name $ \win -> do
        initRenderer
        glClearColor 0.125 0.125 0.125 1
        glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA
        glActiveTexture gl_TEXTURE0
                
        glEnable gl_PROGRAM_POINT_SIZE -- for OSX

        (width, height) <- GLFW.getFramebufferSize win

        let scrSize = (Size width height)

        resources <- loadResources "resources"
        inputPoller <- makeInputPoller win

        let go mdl prev_time = do

            current_time <- getCurrentTime
            let delta = min 0.1 $ realToFrac (diffUTCTime current_time prev_time)

            input <- inputPoller
            let msgs = catMaybes $ map makeMsg input

            glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)

            let mat = calcCameraMatrix scrSize mdl
            render resources (RenderInfo mat scrSize worldLayer) mdl
            renderCommands mat worldLayer

            resetRenderer
            GLFW.swapBuffers win

            let mdl' = physics delta $ foldl' (flip update) mdl msgs

            go mdl' current_time

        t <- getCurrentTime
        go newGame t

main :: IO ()
main = do
        doglfw "Demo"
               (Size 480 640)
