module ClassTest where

import Test.Tasty.HUnit
import PlantUml.ClassDiagram

unit_simpleClass :: IO ()
unit_simpleClass = classOf "Main" @?= Class "Main"

unit_ClassWithProperties :: IO ()
unit_ClassWithProperties = classProps "User" [("name", "John")] @?= ClassWithProps "User" [("name", "John")]