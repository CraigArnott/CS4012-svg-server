{-# LANGUAGE OverloadedStrings #-}

module Main where
import Convert
import Shapes

main :: IO ()
main = do
  writeFile "test.svg" $ convert $ buildDrawing

buildDrawing :: Drawing
buildDrawing = Drawing [(Identity, Square, [Fill green, Width 1, Height 5, Stroke blue])]

