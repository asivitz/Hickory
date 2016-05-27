{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}

module React.React where

import Data.Dynamic
import Data.Maybe

-- Library

conv :: Typeable a => Dynamic -> a
conv = fromJust . fromDynamic

data Component gfx = Stateless ([Component gfx] -> gfx) [Component gfx]
               | Stateful {
                          step :: (Dynamic -> Dynamic),
                          snapshot :: Dynamic,
                          renderFunc :: Dynamic -> [Component gfx] -> gfx,
                          children :: [Component gfx]
                          }

instance Show (Component gfx) where
        show Stateful { snapshot, children } = "Component { state: " ++ show snapshot ++ ", children: " ++ show children ++ " }"
        show (Stateless _ children)  = "Component { children: " ++ show children ++ " }"

stepComp :: Component gfx -> Component gfx
stepComp Stateful { step, snapshot, renderFunc, children } = Stateful step (step snapshot) renderFunc (map stepComp children)
stepComp (Stateless render children) = Stateless render (map stepComp children)

renderComp :: Component gfx -> gfx
renderComp Stateful { snapshot, renderFunc, children } = renderFunc snapshot children
renderComp (Stateless renderFunc children) = renderFunc children

mkTerminal :: gfx -> Component gfx
mkTerminal tree = Stateless (\_ -> tree) []


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
