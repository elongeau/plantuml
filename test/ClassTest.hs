{-# LANGUAGE FlexibleInstances #-}

module ClassTest where

import Test.Tasty.HUnit
import PlantUml.ClassDiagram
import Control.Monad.State

unit_CanUseDoNotation :: IO ()
unit_CanUseDoNotation = 
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