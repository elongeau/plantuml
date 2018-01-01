{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module PlantUml.ClassDiagram where

newtype Diagram a =
   Diagram [a]
   deriving (Show, Eq)

data Class = 
    Class { className :: String }
    deriving (Show, Eq)

data Relation =
    Link { from :: Class, to :: Class }
    deriving (Show, Eq)

instance Functor Diagram where
    fmap f (Diagram xs) = Diagram $ fmap f xs

class Plantuml a where
    toPlantuml :: a -> String

instance Plantuml Class where
    toPlantuml (Class c) = 
        "class " ++ c

instance Plantuml Relation where
    toPlantuml (Link (Class f) (Class t)) = 
        f ++ " -> " ++ t

data Plantumlable =
    forall a. (Plantuml a, Show a) => Plantumlable a

instance Show Plantumlable where
    show (Plantumlable p) = show p 

instance Plantuml Plantumlable where
    toPlantuml (Plantumlable a) = toPlantuml a

instance Applicative Diagram where
    pure d = Diagram [ d ]
    (Diagram fs) <*> Diagram (xs) = Diagram $ fs <*> xs
