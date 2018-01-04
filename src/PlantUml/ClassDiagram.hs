{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module PlantUml.ClassDiagram where

data Diagram = Diagram {
    items :: [Item],
    links :: [Link]
}

data Item =
    Class String
    deriving (Eq, Show)

data Link = 
    Link Item Item
    deriving (Eq, Show)

initialDiagram :: Diagram
initialDiagram =
    Diagram [] []

addClass :: Diagram -> String -> Diagram
addClass (Diagram items links) className = 
    Diagram { items = Class className : items, links = links}
    
class PlantUmlable a where
    toPlantuml :: a -> String
    
instance PlantUmlable Diagram where
    toPlantuml :: Diagram -> String
    toPlantuml (Diagram items links) = 
        let
            items' = unlines . map toPlantuml $ items
        in
            items'

instance PlantUmlable Item where
    toPlantuml :: Item -> String
    toPlantuml (Class className) = 
        "class " ++ className