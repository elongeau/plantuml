{-# LANGUAGE FlexibleInstances #-}

module ClassTest where

import Test.Tasty.HUnit
import PlantUml.ClassDiagram
    
instance Plantuml [Char] where
    toPlantuml = id

unit_create_a_simple_diagram :: IO ()
unit_create_a_simple_diagram = 
    res @?= Diagram [ "class Foo" ]
        where res = toPlantuml <$> Diagram [ Class "Foo" ]

unit_create_two_linked_class :: IO ()
unit_create_two_linked_class = 
    let
        foo = Class "Foo"
        bar = Class "Bar"
        rel = Plantumlable $ Link foo bar
        diag = toPlantuml <$> Diagram [ Plantumlable $ foo, Plantumlable $ bar, rel ]
    in 
        diag @?= Diagram [
            "class Foo", 
            "class Bar",
            "Foo -> Bar"
        ]