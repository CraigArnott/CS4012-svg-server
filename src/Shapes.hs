module Shapes(
  Shape (..), Point, Vector (..), Transform (..), Drawing (..), Style (..), Stylesheet, Colour (..), 
  point, getX, getY,
  red, green, blue,
  stroke, fill, outline, size,
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
           | Outline Double
           | Size Double
             deriving Show

stroke a = Stroke a
fill = Fill 

outline, size :: Double -> Style
outline x = Outline x
size x = Size x

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

-- Transformations

data Transform = Identity
           | Translate Vector
           | Scale Vector
           | Compose Transform Transform
           | Rotate Double
             deriving Show

identity = Identity
translate = Translate
scale = Scale
rotate = Rotate
--rotate angle = Rotate $ matrix (cos angle) (-sin angle) (sin angle) (cos angle)
t0 <+> t1 = Compose t0 t1

--transform :: Transform -> Point -> Point
--transform Identity                   x = id x
--transform (Translate (Vector tx ty)) (Vector px py)  = Vector (px - tx) (py - ty)
--transform (Scale (Vector tx ty))     (Vector px py)  = Vector (px / tx)  (py / ty)
--transform (Rotate m)                 p = (invert m) `mult` p
--transform (Compose t1 t2)            p = transform t2 $ transform t1 p

-- Drawings

data Drawing = Drawing [(Transform,Shape,Stylesheet)]
drawing = Drawing
