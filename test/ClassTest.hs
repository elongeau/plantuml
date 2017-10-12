module ClassTest where

import Test.Tasty.HUnit

data Diagram = Diagram { links :: [(Class, Class)] }
    deriving (Eq,Show)
data Class = Class { name :: String }
    deriving (Eq)

instance Show Class where
    show (Class name) = "class " ++ name
simpleCase :: Diagram
simpleCase = 
    let
        foo = Class "Foo"
        bar = Class "Bar"
    in
        Diagram [(foo,bar)]

plantUML :: Diagram -> [String]
plantUML (Diagram links) = 
    let 
        first = map (toName fst) links
        second = map (toName snd) links
        allLink = map toString links
    in first ++ second ++ allLink
        where
            toName :: ((Class,Class) -> Class) -> (Class, Class) -> String
            toName f link = show $ f link
            toString :: (Class, Class) -> String
            toString (left, right) = show (name left) ++ " o-- " ++ show (name right)

unit_convert_a_diagram_to_a_string :: IO ()
unit_convert_a_diagram_to_a_string = res @?= unlines ["class Foo","class Bar","Foo o-- Bar"]
        where
            res = unlines $ plantUML simpleCase
                    