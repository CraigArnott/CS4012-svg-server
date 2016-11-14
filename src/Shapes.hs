module Shapes(
  Shape (..), Point, Vector, Transform (..), Drawing (..), Style (..), Stylesheet, Colour (..),
  point, getX, getY,
  empty, circle, square,
  red, green, blue,
  stroke, fill, outline, height, width,
  identity, translate, rotate, scale, (<+>)
  )  where

-- Utilities

data Vector = Vector Double Double
              deriving Show
vector = Vector

cross :: Vector -> Vector -> Double
cross (Vector a b) (Vector a' b') = a * a' + b * b'

mult :: Matrix -> Vector -> Vector
mult (Matrix r0 r1) v = Vector (cross r0 v) (cross r1 v)

invert :: Matrix -> Matrix
invert (Matrix (Vector a b) (Vector c d)) = matrix (d / k) (-b / k) (-c / k) (a / k)
  where k = a * d - b * c
        
-- 2x2 square matrices are all we need.
data Matrix = Matrix Vector Vector
              deriving Show

matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = Matrix (Vector a b) (Vector c d)

getX (Vector x y) = x
getY (Vector x y) = y

-- Styling

type Stylesheet = [Style]

data Style = Stroke Colour
           | Fill Colour
           | Outline Int
           | Height Int
           | Width Int
             deriving Show

stroke, fill :: Colour -> Style
stroke c = Stroke c
fill c = Fill c

outline, height, width :: Int -> Style
outline x = Outline x
height x = Height x
width x = Width x

-- Colours

data Colour = Red
            | Green
            | Blue
              deriving Show

red, green, blue :: Colour

red = Red
green = Green
blue = Blue

-- Shapes

type Point  = Vector

point :: Double -> Double -> Point
point = vector

data Shape = Empty 
           | Circle 
           | Square
             deriving Show

empty, circle, square :: Shape

empty = Empty
circle = Circle
square = Square

-- Transformations

data Transform = Identity
           | Translate Vector
           | Scale Vector
           | Compose Transform Transform
           | Rotate Matrix
             deriving Show

identity = Identity
translate = Translate
scale = Scale
rotate angle = Rotate $ matrix (cos angle) (-sin angle) (sin angle) (cos angle)
t0 <+> t1 = Compose t0 t1

transform :: Transform -> Point -> Point
transform Identity                   x = id x
transform (Translate (Vector tx ty)) (Vector px py)  = Vector (px - tx) (py - ty)
transform (Scale (Vector tx ty))     (Vector px py)  = Vector (px / tx)  (py / ty)
transform (Rotate m)                 p = (invert m) `mult` p
transform (Compose t1 t2)            p = transform t2 $ transform t1 p

-- Drawings

data Drawing = Drawing [(Transform,Shape,Stylesheet)]

