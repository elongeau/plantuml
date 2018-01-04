{-# LANGUAGE FlexibleInstances #-}

module ClassTest where

import Test.Tasty.HUnit
import PlantUml.ClassDiagram
    
unit_create_a_diagram_with_one_class :: IO ()
unit_create_a_diagram_with_one_class = 
    let
        d = addClass initialDiagram "Foo"
        uml = toPlantuml d
    in
        uml @?= unlines [ "class Foo" ]
