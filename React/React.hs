{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}

module React.React where

import Data.Dynamic
import Data.Maybe

-- Library

conv :: Typeable a => Dynamic -> a
conv x = case fromDynamic x of
             Just s -> s
             Nothing -> error ("Wrong state type found: " ++ show x)

data Component gfx input = Stateless ([Component gfx input] -> gfx) [Component gfx input]
               | Stateful {
                          step :: (Dynamic -> Dynamic),
                          snapshot :: Dynamic,
                          renderFunc :: Dynamic -> [Component gfx input] -> gfx,
                          children :: [Component gfx input],
                          inputFunc :: input -> Dynamic -> Dynamic,
                          debugPrint :: Dynamic -> String
                          }

instance Show (Component gfx input) where
        show Stateful { snapshot, children, debugPrint } = let val = debugPrint snapshot in
            "Component { state: " ++ show val ++ ", children: " ++ show children ++ " }"
        show (Stateless _ children)  = "Component { children: " ++ show children ++ " }"

stepComp :: Component gfx input -> Component gfx input
stepComp comp@Stateful { step, snapshot, renderFunc, children, inputFunc } = comp { snapshot = (step snapshot), children = (map stepComp children) }
stepComp (Stateless render children) = Stateless render (map stepComp children)

renderComp :: Component gfx input -> gfx
renderComp Stateful { snapshot, renderFunc, children } = renderFunc snapshot children
renderComp (Stateless renderFunc children) = renderFunc children

mkTerminal :: gfx -> Component gfx input
mkTerminal tree = Stateless (\_ -> tree) []

inputComp :: input -> Component gfx input -> Component gfx input
inputComp input (Stateless rfunc cs) = Stateless rfunc (map (inputComp input) cs)
inputComp input c@Stateful { snapshot, inputFunc, children } = c { snapshot = inputFunc input snapshot, children = map (inputComp input) children }


{-
-- Presentation

type V2 = (Int, Int)

data RenderTree = Square V2 V2 RenderTree
                | Text V2 String
                | List [RenderTree]
                deriving (Show)

-- App

type Model = [String]

data Msg =
        Push String
        | Pop
        deriving (Show)

update :: Msg -> Model -> Model
update (Push str) model = str : model
update Pop (x:xs) = xs

type Comp = Component RenderTree

view :: Model -> Comp
view xs = mkCanvas (mkButton : map mkLabel xs)

mkButton :: Comp
mkButton = Stateful {
                     step = \x -> toDyn $ conv x + 1,
                     snapshot = toDyn (0 :: Int),
                     renderFunc = \x irs -> Square (conv x, 0) (1,1) (Text (0,0) "Click!"),
                     children = []
                     }

mkCanvas :: [Comp] -> Comp
mkCanvas children = Stateless (\cs -> Square (0,0) (8,8) (List $ map renderComp cs)) children

mkLabel :: String -> Comp
mkLabel str = mkTerminal $ Text (0,0) str
-}
