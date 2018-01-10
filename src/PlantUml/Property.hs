module PlantUml.Property (
    Property,
    private,
    protected,
    packaged,
    public
) where

import PlantUml.Plantumlable

data Property = 
    Property {
        propertyAccessLevel :: AccessLevel,
        propertyType :: String,
        propertyName :: String
    }
    deriving (Eq, Show)

data AccessLevel = 
    Private
    | Protected
    | Packaged
    | Public
    deriving (Eq, Show)

private :: String -> String -> Property
private = Property Private

protected :: String -> String -> Property
protected = Property Protected

packaged :: String -> String -> Property
packaged = Property Packaged

public :: String -> String -> Property
public = Property Public

instance PlantUmlable Property where
    toPlantuml (Property scope propertyType propertyName) = 
        "  " ++ toPlantuml scope ++ " " ++ propertyType ++ " " ++ propertyName

instance PlantUmlable AccessLevel where
    toPlantuml Private = "-"
    toPlantuml Protected = "#"
    toPlantuml Packaged = "~"
    toPlantuml Public = "+"
