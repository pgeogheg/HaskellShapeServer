{-# LANGUAGE OverloadedStrings #-}
module Main where

import Shapes
import Control.Monad
import System.IO
import Data.Maybe
import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)

test :: Drawing
test = [(Compose Identity (Translate (Vector 5 5)), Circle, [FillColour (Colour 255 0 0), StrokeWidth 2])]

main :: IO ()
main = do
	let b = map renderSvg $ makeAllSvg test
	printAll b
	--let a = renderSvg svgDoc
	--putStrLn a

printAll :: [String] -> IO()
printAll [] = return ()
printAll (x:xs) = do 
	putStrLn x
	printAll xs

svgDoc :: S.Svg
svgDoc = S.docTypeSvg ! A.version "1.1" ! A.width "150" ! A.height "100" ! A.viewbox "0 0 3 2" $ do
    S.g ! A.transform makeTransform $ do
      S.rect ! A.width "1" ! A.height "2" ! A.fill "#008d46"
      S.rect ! A.width "1" ! A.height "2" ! A.fill "#ffffff"
      S.rect ! A.width "1" ! A.height "2" ! A.fill "#d2232c"
      S.path ! A.d makePath

makePath :: S.AttributeValue
makePath = mkPath $ do
  l 2 3
  m 4 5

makeTransform :: S.AttributeValue
makeTransform = S.rotate 50

--VERSION CONTROL 1

makeAllSvg :: Drawing -> [S.Svg]
makeAllSvg d = map makeSvg d 

makeSvg :: Frame -> S.Svg
makeSvg f 	| isJust $ interpretDrawings f = S.docTypeSvg ! A.version "1.1" ! A.width "200" ! A.height "200" $ do fromJust $ interpretDrawings f
			| otherwise = S.docType ! A.version "1.1" ! A.width "200" ! A.height "200" 

interpretDrawings :: Frame -> Maybe S.Svg
interpretDrawings (trans, shape, style)
	| (catMaybes (interpretTransforms trans) == []) = do (foldl (!) (fromJust $ interpretShape shape) (mapMaybe interpretStyle style))
	| (isJust $ interpretShape shape) = Just $ (foldl (!) S.g (fromMaybe $ interpretTransforms trans)) $ do 
		(foldl (!) (fromJust $ interpretShape shape) (mapMaybe interpretStyle style))
	| otherwise = Nothing

interpretTransforms :: Transform -> [Maybe S.Attribute]
interpretTransforms Identity 										= [Nothing]
interpretTransforms (Translate (Vector x y))						= [Just (A.transform $ S.translate x y)]
interpretTransforms (Scale (Vector x y))							= [Just (A.transform $ S.scale x y)]
interpretTransforms (Compose t1 t2)									= interpretTransforms t1 ++ interpretTransforms t2
interpretTransforms (Rotate (Matrix (Vector a b) (Vector c d))) 	= [Just (A.transform $ S.rotate (acos a))]

interpretShape :: Shape -> Maybe S.Svg
interpretShape Circle 	= Just (S.circle ! A.cx "50" ! A.cy "50" ! A.r "50")
interpretShape Square 	= Just (S.rect ! A.x "50" ! A.y "50" ! A.width "50" ! A.height "50")
interpretShape Empty 	= Nothing

interpretStyle :: Style -> Maybe S.Attribute
interpretStyle None 							= Nothing
interpretStyle (StrokeWidth val) 				= Just (A.strokeWidth (S.stringValue (show val)))
interpretStyle (StrokeColour (Colour r g b)) 	= Just (A.stroke (S.stringValue ("rgb(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")")))
interpretStyle (FillColour (Colour r g b)) 		= Just (A.fill (S.stringValue ("rgb(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")")))
