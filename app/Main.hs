module Main where

{-# LANGUAGE OverloadedStrings #-}

import Convert
import Shapes

import Web.Scotty

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as R

import Data.Text.Lazy

main = scotty 3000 $ do
  get (capture "/form") $ do
    file "index.html"
  post (capture "/") $ do
    drawing <- param $ pack "drawing" 
    html $ pack $ convert $ (read drawing :: Drawing)

--main :: IO ()
--main = do
--  writeFile "test.svg" $ convert $ (read buildDrawing :: Drawing)

buildDrawing :: String
buildDrawing = "Drawing [(Compose (Translate (Vector 50 50)) (Rotate 45), Square, [Fill Green, Stroke Blue, Outline 5, Size 100]), (Identity, Circle, [Fill Blue, Size 30])]"
