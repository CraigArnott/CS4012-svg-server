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

