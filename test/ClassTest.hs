{-# LANGUAGE FlexibleInstances #-}

module ClassTest where

import Test.Tasty.HUnit
import PlantUml.ClassDiagram
import Control.Monad.State

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