
module Colorsplicer where

import Dither
import MakeIMG
import Codec.Picture.Types

dynDith = ImageRGB8 picDith

picDith = ditherFloydRGB8 rgbPixls pic


checkColor :: PixelRGB8 -> PixelRGB8 -> Bool
checkColor (PixelRGB8 a1 a2 a3) (PixelRGB8 b1 b2 b3)
    | a1 /= b1  = False
    | a2 /= b2  = False 
    | a3 /= b3  = False
    | otherwise = True

colorSplicer :: Image PixelRGB8 -> PixelRGB8 -> [Point] 
colorSplicer img@(Image { imageWidth  = w, 
                          imageHeight = h }) pix =
    [(x,y)| x <- [0..w-1], y <-[0..h-1], checkColor (pixelAt img x y) pix]

































