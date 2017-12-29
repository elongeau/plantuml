{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module PlantUml.ClassDiagram where

data Diagram a =
   Diagram [a]
   deriving (Show, Eq)

data Class = 
    Class { className :: String }

data Relation = 
    Link { from :: Class, to :: Class }

instance Functor Diagram where
    fmap f (Diagram xs) = Diagram $ fmap f xs

class Plantuml a where
    toPlantuml :: a -> String

instance Plantuml Class where
    toPlantuml = 
        (++) "class " . className

instance Plantuml Relation where
    toPlantuml (Link (Class f) (Class t)) = 
        f ++ " -> " ++ t
data Plantumlable =
    forall a. Plantuml a => Plantumlable a

instance Plantuml Plantumlable where
    toPlantuml (Plantumlable a) = toPlantuml a

