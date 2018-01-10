module PlantUml.Plantumlable where

class PlantUmlable a where
    toPlantuml :: a -> String