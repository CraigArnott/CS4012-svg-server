{-# LANGUAGE OverloadedStrings #-}

module Main where
import Convert
import Shapes

main :: IO ()
main = do
  writeFile "test.svg" $ convert $ buildDrawing

buildDrawing :: Drawing
buildDrawing = Drawing [(Translate (Vector 50 50) <+> Rotate 45, Square, [Fill green, Stroke blue, Outline 5, Size 100]), (Identity, Circle, [Fill blue, Size 0.1])]
