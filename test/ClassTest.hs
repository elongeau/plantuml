{-# LANGUAGE FlexibleInstances #-}

module ClassTest where

import Test.Tasty.HUnit
import PlantUml.ClassDiagram
    
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