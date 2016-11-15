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
import Web.Scotty
import qualified Data.Text.Lazy as L

test :: Int -> Drawing
test i 	| i == 1 = [(Compose (Scale (Vector 2 2)) (Translate (Vector 5 5)), Circle, [FillColour (Colour 255 0 0), StrokeWidth 2])]
		| i == 2 = [(Identity, Empty, [None])]

main :: IO ()
main = scotty 3000 $ do
	let c = L.pack (renderSvg $ interpretDrawing (test 1))
	get "/" $ do 
		html c
		webForm

	get "/shape/:text" $ do
		text <- param "text"
		html text

---------------------
--   Web Service   --
---------------------

webForm :: ActionM ()
webForm = do html $ L.pack "<form action=\"/shape/\" method=\"POST\">First name:<br><input type=\"text\" name=\"firstname\" value=\"Mickey\"><br>Last name:<br><input type=\"text\" name=\"lastname\" value=\"Mouse\"><br><br><input type=\"submit\" value=\"Submit\"></form>"

---------------------
-- SVG Interpreter --
---------------------

interpretDrawing :: Drawing -> S.Svg
interpretDrawing draw@((_, shape, _):_)	| isNothing (interpretShape shape) = S.docType ! A.version "1.1" ! A.width "200" ! A.height "200"
										| otherwise = S.docTypeSvg ! A.version "1.1" ! A.width "200" ! A.height "200" $ do runSvgs (map interpretFrame draw)

runSvgs :: [S.Svg] -> S.Svg
runSvgs svgs = foldl1 (>>) svgs

interpretFrame :: Frame -> S.Svg
interpretFrame (trans, shape, style) 
	| null (interpretTransforms trans) && length (interpretStylesheet style) > 0		= foldl (!) (fromJust (interpretShape shape)) (interpretStylesheet style)
	| length (interpretTransforms trans) > 0 && length (interpretStylesheet style) > 0 	= foldl (!) (foldl (!) (fromJust (interpretShape shape)) (interpretTransforms trans)) (interpretStylesheet style)
	| length (interpretTransforms trans) > 0 && null (interpretStylesheet style) 		= foldl (!) (fromJust (interpretShape shape)) (interpretTransforms trans)

interpretTransforms :: Transform -> [S.Attribute]
interpretTransforms trans = catMaybes (interpretTransforms1 trans)

interpretTransforms1 :: Transform -> [Maybe S.Attribute]
interpretTransforms1 Identity 										= [Nothing]
interpretTransforms1 (Translate (Vector x y))						= [Just (A.transform $ S.translate x y)]
interpretTransforms1 (Scale (Vector x y))							= [Just (A.transform $ S.scale x y)]
interpretTransforms1 (Rotate (Matrix (Vector a b) (Vector c d))) 	= [Just (A.transform $ S.rotate (acos a))]
interpretTransforms1 (Compose t1 t2)								= interpretTransforms1 t1 ++ interpretTransforms1 t2

interpretShape :: Shape -> Maybe S.Svg
interpretShape Circle 	= Just (S.circle ! A.cx "50" ! A.cy "50" ! A.r "50")
interpretShape Square 	= Just (S.rect ! A.x "50" ! A.y "50" ! A.width "50" ! A.height "50")
interpretShape Empty 	= Nothing

interpretStylesheet :: Stylesheet -> [S.Attribute]
interpretStylesheet sheet = catMaybes (map interpretStyle sheet)

interpretStyle :: Style -> Maybe S.Attribute
interpretStyle None 							= Nothing
interpretStyle (StrokeWidth val) 				= Just (A.strokeWidth (S.stringValue (show val)))
interpretStyle (StrokeColour (Colour r g b)) 	= Just (A.stroke (S.stringValue ("rgb(" ++ show r ++ "," ++ show g ++  "," ++ show b ++ ")")))
interpretStyle (FillColour (Colour r g b)) 		= Just (A.fill (S.stringValue ("rgb(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")")))
