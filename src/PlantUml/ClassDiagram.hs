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
addClass d@(Diagram items _) className = 
    d { items = Class className : items}
    
addLink :: Diagram -> Item -> Item -> Diagram
addLink d@(Diagram _ links) i1 i2 =
    d { links = Link i1 i2 : links }

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