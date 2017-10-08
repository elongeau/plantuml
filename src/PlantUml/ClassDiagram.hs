module PlantUml.ClassDiagram where

data ClassDiagram = Class {className :: String} 
    | ClassWithProps {className :: String, props :: [(String,String)]}
    deriving (Eq, Show)

classOf :: String -> ClassDiagram
classOf clazz = Class clazz

classProps :: String -> [(String,String)] -> ClassDiagram
classProps clazz props = ClassWithProps clazz props
