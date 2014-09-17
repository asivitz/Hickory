{-# LANGUAGE NamedFieldPuns #-}

module Freecell.MenuScene where

import Engine.Scene.Scene
import Engine.Scene.Input
import Engine.Component.Model
import Engine.Component.Component
import Freecell.Events
import Data.IORef
import Camera.Camera
import Types.Types
import Math.Matrix
import Graphics.Drawing
import Math.Vector
import Menus.Menus
import Systems.DrawText
import Systems.Draw
import Types.Color
import Graphics.DrawText

type Screen = MenuScreen Scalar InputEvent

data ComponentStore = ComponentStore

data Resources = Resources {
               pvcShader :: Shader,
               printer :: Printer Int
               }

type MenuModel = Model ComponentStore (TransitionStack Screen)

font = "goudy_bookletter_1911"

loadResources resPath = do
        shader <- loadShader resPath (fst pvcShaderPair) (snd pvcShaderPair)
        case shader of
            Nothing -> error "Couldn't load PVC Shader"
            Just sh -> do
                pr <- loadPrinter resPath sh font
                case pr of
                    Nothing -> error "Couldn't load printer"
                    Just p -> return $ Resources { printer = p, pvcShader = sh }

processInput :: RenderInfo -> InputEvent -> MenuModel -> (MenuModel, [InputEvent])
processInput ri ie model = (model, [])

stepModel :: Double -> MenuModel -> MenuModel
stepModel delta model@Model { _game = TransitionStack stk time leaving } = 
        model { _game = TransitionStack stk (time + delta) leaving }

resolveMenuItem screenSize fract incoming (rvecF, tmdcF) =
        let pos = screenPos screenSize (rvecF incoming fract)
            tmdc = tmdcF incoming fract in
                case tmdc of
                    TextMenuDrawCommand dc -> PositionedTextCommand pos dc

elementToPositionedTextCommands :: Real a => Size Int -> Bool -> Double -> UIElement a c t -> [PositionedTextCommand]
elementToPositionedTextCommands screenSize incoming fract (UIElement _ menuItems) = map (resolveMenuItem screenSize fract incoming) menuItems

render :: Resources -> RenderInfo -> MenuModel -> IO ()
render Resources { pvcShader, printer } (RenderInfo _ ss label) Model { _game = transitionStack } = 
        case incomingScreen transitionStack of
            Just (MenuScreen elements duration) -> let fract = min 1 $ (transitionTime transitionStack) / duration
                                                       commands = concat $ map (elementToPositionedTextCommands ss True fract) elements in
                printCommands pvcShader label printer commands
            Nothing -> return ()

makeButton rRect events action items =
        UIElement (Just (Button rRect (events, action))) items

mainMenu :: MenuScreen Scalar InputEvent
{-mainMenu = MenuScreen [simpleMenuButton 0 "New Game" PopScreen _newGame] 0.5-}
mainMenu = MenuScreen [makeButton (RRect (RVec (center 0) (beg 40)) (RVec (end 40) (beg 30)))
                                  []
                                  (Just $ pushScreen subMenu)
                                  [((\incoming fract -> RVec (RScal (fract * 0.5) 0) (beg 40)), 
                                  (\incoming fract -> TextMenuDrawCommand textcommand { text = "Main Menu", fontSize = 6, color = rgba 1 1 1 1}))]
                                  ]
                                  0.5

subMenu :: MenuScreen Scalar InputEvent
{-mainMenu = MenuScreen [simpleMenuButton 0 "New Game" PopScreen _newGame] 0.5-}
subMenu = MenuScreen [makeButton (RRect (RVec (center 0) (beg 40)) (RVec (end 40) (beg 30)))
                                  []
                                  (Just popScreen)
                                  [((\incoming fract -> RVec (RScal (fract * 0.5) 0) (beg 40)), 
                                  (\incoming fract -> TextMenuDrawCommand textcommand { text = "Back", fontSize = 6, color = rgba 1 1 1 1}))]
                                  ]
                                  0.5

{-
simpleMenuButton :: Int -> String -> ScreenAction Scalar (GameEvent IO) -> [GameEvent IO] -> UIElement Scalar (GameEvent IO)
simpleMenuButton idx txt action events = UIElement (Just (Button (RRect (center 0, beg 40) (end 40, beg 30)) (events, Just action))) $ 
    MenuRenderSpec ([], [font], []) $ \(MenuResources _ [pid] _) ->
        \fraction incoming ->
            let frac' = constrainInterval fraction idx in
            [(beg (40 * (realToFrac (1 + idx))), center 0, 
                TextMenuDrawCommand pid DrawText.textcommand { text = txt, fontSize = 6, color = rgba 1 1 1 frac' })]
                -}

makeScene = do
        is <- newIORef (Input [])
        let cam = \(Size w h) -> Camera (Ortho (realToFrac w) (-20) 1) (Route pZero Nothing)
            scene = Scene {
                          _name = "Menu",
                          _model = newModel cam ComponentStore (pushScreen mainMenu emptyTransitionStack),
                          _renderInfo = RenderInfo mat44Identity nullSize uiLabel,
                          _loadResources = loadResources "Example/HFreecell/resources",
                          _stepModel = makeStepModel processInput stepModel,
                          _render = render,
                          _inputStream = is,
                          _loadedRender = Nothing }
        return scene
