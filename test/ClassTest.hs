{-# LANGUAGE FlexibleInstances #-}

module ClassTest where

import Test.Tasty.HUnit
import PlantUml.ClassDiagram

import Control.Monad.State
    
unit_CreateADiagramWithOneClass :: IO ()
unit_CreateADiagramWithOneClass = 
    let
        d = addClass initialDiagram "Foo"
        uml = toPlantuml d
    in
        uml @?= unlines [ "class Foo" ]

unit_CreateADiagramWithLinkedClass :: IO ()
unit_CreateADiagramWithLinkedClass = 
    let
        d = addClass initialDiagram "Foo"
        d' = addClass d "Bar"
        d'' = addLink d' (Class "Foo") (Class "Bar")
        uml = toPlantuml d''
    in
        uml @?= unlines [
            "class Foo",
            "class Bar",
            "Foo -> Bar"
        ]

type DiagramState a = State Diagram a

clazz :: String -> DiagramState Item
clazz className =
    state $ \d -> (Class className, addClass d className)

link :: Item -> Item -> DiagramState Link
link i1 i2 =
    state $ \d -> (Link i1 i2, addLink d i1 i2)

unit_CanUseDoNotation :: IO ()
unit_CanUseDoNotation = 
    let
        d = do
                foo <- clazz "Foo"
                bar <- clazz "Bar"
                link foo bar
        res = execState d initialDiagram
        uml = toPlantuml res
    in
        uml @?= unlines [ 
            "class Foo",
            "class Bar",
            "Foo -> Bar"
        ]