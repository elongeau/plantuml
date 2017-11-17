module ClassTest where

import Test.Tasty.HUnit

data ClassDiagram = 
    ClassDiagram [ Entity ]

data Entity = 
    Box Class
    | Link Relation
    
data Class = 
    Class { name :: String }

data Relation = 
    Relation Class Class 
    deriving Show

classOf :: String -> Class
classOf name = Class name

box :: Class -> Entity
box clazz = Box clazz

instance Show Class where
    show (Class name) = "class " ++ name 

instance Show Entity where 
    show (Box clazz) = show clazz
    show (Link rel) = show rel

instance Show ClassDiagram where
    show (ClassDiagram entities) = unlines $ map show entities

unit_create_a_simple_diagram :: IO ()
unit_create_a_simple_diagram = 
    res @?= unlines [ "class Foo" ]
        where res = show $ ClassDiagram [ box $ classOf "Foo" ]
