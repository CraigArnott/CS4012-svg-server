module Shapes(
  Shape (..), Point, Vector (..), Transform (..), Drawing (..), Style (..), Stylesheet, Colour (..), 
  identity, translate, rotate, scale, (<+>)
  )  where

-- Utilities

data Vector = Vector Double Double
              deriving (Show, Read)
vector = Vector

cross :: Vector -> Vector -> Double
cross (Vector a b) (Vector a' b') = a * a' + b * b'

-- Styling

type Stylesheet = [Style]

data Style = Stroke Colour
           | Fill Colour
           | Outline Double
           | Size Double
             deriving (Show, Read)

-- Colours

data Colour = Red
            | Green
            | Blue
            | Hex String
              deriving (Show, Read)

-- Shapes

type Point  = Vector

point :: Double -> Double -> Point
point = vector

data Shape = Empty 
           | Circle 
           | Square
             deriving (Show, Read)

-- Transformations

data Transform = Identity
           | Translate Vector
           | Scale Vector
           | Compose Transform Transform
           | Rotate Double
             deriving (Show, Read)

identity = Identity
translate = Translate
scale = Scale
rotate = Rotate
t0 <+> t1 = Compose t0 t1

-- Drawings

data Drawing = Drawing [(Transform,Shape,Stylesheet)]
               deriving (Show, Read)
