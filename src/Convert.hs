{-# LANGUAGE OverloadedStrings #-}

module Convert(convert)  where

import Data.List (intersperse)
import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)
import Shapes
import System.IO

-- convert drawing to svg

convert :: Drawing -> String
convert (Drawing x) = renderSvg $ docHeader $ foldl1 (>>) $ map convertGraphic x

-- convert triple to graphic

convertGraphic :: (Transform, Shape, Stylesheet) -> S.Svg
convertGraphic (trans, shape, sheet) = let t = buildTransform trans in
                                       case t of 
                                        (Nothing) -> convertShape shape sheet
                                        (Just a) -> (convertShape shape sheet) ! a

convertShape :: Shape -> Stylesheet -> S.Svg 
convertShape shape sheet = foldl (!) (shapeToSvg shape) (parseStylesheet shape sheet)

-- create header for svg doc

docHeader :: S.Svg -> S.Svg
docHeader = S.docTypeSvg ! A.version "1.1" ! A.width "1500" ! A.height "1000" ! A.viewbox "0 0 1500 1000"

-- parsing transforms

buildTransform :: Transform -> (Maybe S.Attribute)
buildTransform Identity = Nothing
buildTransform x = Just $ A.transform $ mconcat $ intersperse " " $ parseTransform x  

parseTransform :: Transform -> [S.AttributeValue]
parseTransform Identity = []
parseTransform (Rotate x) = [S.rotate x]
parseTransform (Translate (Vector x y )) = [S.translate x y]
parseTransform (Scale (Vector x y)) = [S.scale x y]
parseTransform (Compose x y) = (parseTransform x) ++ (parseTransform y)

-- parsing stylesheets

parseStylesheet :: Shape -> Stylesheet -> [S.Attribute] 
parseStylesheet shape sheet = concat $ map (styleToAttrs shape) sheet 

styleToAttrs :: Shape -> Style -> [S.Attribute]
styleToAttrs _ (Stroke c) = [A.stroke (colourToHex c)]
styleToAttrs _ (Fill c) = [A.fill (colourToHex c)]
styleToAttrs _ (Outline x) = [A.strokeWidth $ S.toValue x]
styleToAttrs Circle (Size x) = [A.r $ S.toValue x]
styleToAttrs Square (Size x) = [(A.height $ S.toValue x), (A.width $ S.toValue x)]

colourToHex :: Colour -> S.AttributeValue
colourToHex Red = "#ff0000"
colourToHex Green = "#00ff00"
colourToHex Blue = "#0000ff"

-- parsing shapes

shapeToSvg :: Shape -> S.Svg
shapeToSvg Square = S.rect
shapeToSvg Circle = S.circle

