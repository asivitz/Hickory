{- shooter3.hs
 - This game has simple character movement with the arrow keys,
 - as well as shooting simple missiles with the SpaceBar.
 -}

{-# LANGUAGE NamedFieldPuns #-}

import Debug.Trace

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

import Data.Bits
import Platforms.GLFW.Utils
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB.GeometryShader4

-- Our game data
data Model = Model { 
           playerPos :: V2,
           firingDirection :: V2,
           missiles :: [(V2, V2)]
           }

-- By default, or firingDirection is to the right
newGame :: Model
newGame = Model vZero (v2 1 0) []

-- Our event type
type Event = RawInput

-- We need to determine from our input whether we should fire a missile
data GameInput = GameInput {
               movementVec :: (Maybe V2),
               didFire :: Bool
               }

buildVecWithKeys :: V2 -> (Key, Double) -> V2
buildVecWithKeys vec (key, heldTime) = 
    vec + (case key of
                Key'Up -> v2 0 1
                Key'Left -> v2 (-1) 0
                Key'Down -> v2 0 (-1)
                Key'Right -> v2 1 0
                _ -> vZero)

collectInput :: [Event] -> GameInput
collectInput events = foldl process (GameInput Nothing False) events
    where process gameInput (InputKeysHeld hash) = 
            let moveVec = foldl buildVecWithKeys vZero (HashMap.toList hash)
                in gameInput { movementVec = (Just moveVec) }
          process gameInput (InputKeyDown Key'Space) = gameInput { didFire = True }
          process gameInput _ = gameInput

playerMovementSpeed = 100
missileMovementSpeed = 200

missileInBounds :: (V2, V2) -> Bool
missileInBounds (pos, _) = vmag pos < 500

stepModel :: RenderInfo -> [Event] -> Double -> Model -> (Model, [Event])
stepModel renderinfo events delta Model { playerPos, firingDirection, missiles } =
        let GameInput { movementVec, didFire } = collectInput events 
            (fireDir', playerPos') = case movementVec of
                         Nothing -> (firingDirection, playerPos)
                         -- If we move, then the firingDirection should
                         -- change as well
                         Just v | vnull v -> (firingDirection, playerPos)
                         Just v -> (v, playerPos + (v |* (delta * playerMovementSpeed)))

            -- Fire ze missiles!
            missiles' = if didFire
                            then let newMissile = (playerPos', fireDir') 
                                     in (newMissile : missiles)
                            else missiles

            -- and finally we move all of the missiles
            movedMissiles = filter missileInBounds $ map (\(pos, dir) -> (pos + (dir |* (delta * missileMovementSpeed)), dir)) missiles'
                in (Model { playerPos = playerPos', firingDirection = fireDir', missiles = movedMissiles }, [])

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

        let model = newGame

        let go = do
            input <- inputPoller

            glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)

            let mat = calcCameraMatrix scrSize model
            render resources (RenderInfo mat scrSize worldLayer) model
            renderCommands mat worldLayer

            resetRenderer
            GLFW.swapBuffers win

            go
        go

main :: IO ()
main = do
        doglfw "Demo"
               (Size 480 640)
