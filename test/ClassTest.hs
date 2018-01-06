{-# LANGUAGE FlexibleInstances #-}

module ClassTest where

import Test.Tasty.HUnit
import PlantUml.ClassDiagram

import Control.Monad.State
    
type DiagramState a = State Diagram a

clazz :: String -> DiagramState Item
clazz className =
    state $ \d -> 
        let
            c = Class className 
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

unit_CanUseDoNotation :: IO ()
unit_CanUseDoNotation = 
    let
        d = do
                foo <- clazz "Foo"
                bar <- clazz "Bar"
                link foo bar
        uml = toPlantuml . execState d $ initialDiagram
    in
        uml @?= unlines [ 
            "class Foo",
            "class Bar",
            "Foo -> Bar"
        ]