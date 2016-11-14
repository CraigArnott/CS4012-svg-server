{-# LANGUAGE OverloadedStrings #-}

module Convert(doSample, convert)  where

import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)
import Shapes
import System.IO

-- convert drawing to svg

convert :: Drawing -> String
convert (Drawing x) = renderSvg $ docHeader $ foldl1 (>>) $ map convertShape x

-- convert shape to svg

convertShape :: (Transform, Shape, Stylesheet) -> S.Svg
convertShape (_, shape, sheet) = foldl (!) (shapeToSvg shape) (parseStylesheet sheet)

-- create header for svg doc

docHeader :: S.Svg -> S.Svg
docHeader = S.docTypeSvg ! A.version "1.1" ! A.width "150" ! A.height "100" ! A.viewbox "0 0 3 2"

-- parsing transforms

-- parsing stylesheets

parseStylesheet :: Stylesheet -> [S.Attribute]
parseStylesheet s = map (styleToAttr) s 

styleToAttr :: Style -> S.Attribute
styleToAttr (Stroke c) = A.stroke (colourToHex c)
styleToAttr (Fill c) = A.fill (colourToHex c)
--styleToAttr (Outline x) = A.
styleToAttr (Height x) = A.height $ S.toValue x
styleToAttr (Width x) = A.width $ S.toValue x

colourToHex :: Colour -> S.AttributeValue
colourToHex Red = "#ff0000"
colourToHex Green = "#00ff00"
colourToHex Blue = "#0000ff"

-- parsing shapes

shapeToSvg :: Shape -> S.Svg
shapeToSvg Square = S.rect
shapeToSvg Circle = S.circle

-- sample code

doSample :: IO ()
doSample = do
  writeFile "test.svg" $ renderSvg svgDoc

svgDoc :: S.Svg
svgDoc = S.docTypeSvg ! A.version "1.1" ! A.width "150" ! A.height "100" ! A.viewbox "0 0 3 2" $
      (S.rect ! A.width "1" ! A.height "2" ! A.fill "#008d46") >>
      (S.rect ! A.width "1" ! A.height "2" ! A.fill "#ffffff") >>
      (S.rect ! A.width "1" ! A.height "2" ! A.fill "#d2232c")
