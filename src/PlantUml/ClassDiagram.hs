{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module PlantUml.ClassDiagram (
    clazz,
    clazz',
    link,
    toPlantuml,
    private,
    protected,
    packaged,
    public
)
where

import Control.Monad.State
import PlantUml.Property
import PlantUml.Plantumlable

data Diagram = Diagram {
    items :: [Item],
    links :: [Link]
}

data Item =
    Class {
        className :: String,
        properties :: [Property]
    }
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
    toPlantuml (Class className []) =
        "class " ++ className 
    toPlantuml (Class className properties) = 
            "class " ++ className ++ " {\n" ++
            toPlantuml properties ++
            "}"


instance PlantUmlable Link where
    toPlantuml (Link (Class c1 _) (Class c2 _)) =
        c1 ++ " -> " ++ c2

type DiagramState a = State Diagram a

instance PlantUmlable (DiagramState a) where
    toPlantuml s = 
        toPlantuml . execState s $ initialDiagram

clazz :: String -> DiagramState Item
clazz className =
    clazz' className []

clazz' :: String -> [Property] -> DiagramState Item
clazz' className properties =
    state $ \d ->
        let
            c = Class className properties
            d' = addItem d c
        in
            (c, d')

link :: Item -> Item -> DiagramState Link
link i1 i2 =
    state $ \d ->
        let
            l = Link i1 i2
            d' = addLink d l
        in
            (l, d')
