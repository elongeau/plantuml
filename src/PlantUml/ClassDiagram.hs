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

addItem :: Diagram -> Item -> Diagram
addItem d@(Diagram items _) item = 
    d { items = item : items}
    
addLink :: Diagram -> Link -> Diagram
addLink d@(Diagram _ links) link =
    d { links = link : links }

class PlantUmlable a where
    toPlantuml :: a -> String
    
instance PlantUmlable Diagram where
    toPlantuml (Diagram items links) = 
        let
            items' = toPlantuml items
            links' = toPlantuml links
        in
            items' ++ links'

instance PlantUmlable a => PlantUmlable [a] where
    toPlantuml = 
        unlines . reverse . map toPlantuml

instance PlantUmlable Item where
    toPlantuml (Class className) = 
        "class " ++ className

instance PlantUmlable Link where
    toPlantuml (Link (Class c1) (Class c2)) =
        c1 ++ " -> " ++ c2