{-# LANGUAGE FlexibleInstances #-}

module ClassTest where

import Test.Tasty.HUnit
import PlantUml.ClassDiagram

unit_CreateASimpleDiagram :: IO ()
unit_CreateASimpleDiagram = 
    let
        uml = toPlantuml $ do
            foo <- clazz "Foo"
            bar <- clazz "Bar"
            link foo bar
    in
        uml @?= unlines [ 
            "class Foo",
            "class Bar",
            "Foo -> Bar"
        ]

unit_ClassHaveProperties :: IO ()
unit_ClassHaveProperties = 
    let
        properties = [
            Property Private "String" "bar"
            ]
        uml = toPlantuml $ do
            foo <- clazz' "Foo" properties
            bar <- clazz' "Bar" [
                Property Private "Int" "i"
                ]
            link foo bar
    in
        uml @?= unlines [
            "class Foo {",
            "  - String bar",
            "}",
            "class Bar {",
            "  - Int i",
            "}",
            "Foo -> Bar"
        ]