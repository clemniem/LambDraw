module MakeIMG where

import Codec.Picture
import Codec.Picture.Types

-- Type & Data declarations
type Point      = (Int,Int)
type Distance   = Float
data PixError   = PixError Int Int Int 
    deriving (Show,Eq,Ord)
--type BasID      = Int
--type ErrorFac   = Int

-- HelperVariables for Testing
redPix     = PixelRGB8 255 0   0
greenPix   = PixelRGB8 0   255 0
bluePix    = PixelRGB8 0   0   255
blackPix   = PixelRGB8 0   0   0
whitePix   = PixelRGB8 255 255 255
yellowPix  = PixelRGB8 255 255 0
magentaPix = PixelRGB8 255 0   255
cyanPix    = PixelRGB8 0   255 255

-- PixelLists for rgb and cmyk
rgbPixls   = [redPix,greenPix,bluePix,blackPix,whitePix]
cmykPixls  = [cyanPix,magentaPix,yellowPix,blackPix,whitePix]


pixelung :: Int -> Int -> PixelRGB8
pixelung x y = PixelRGB8 u v (u*v)
    where
        u = fromIntegral x*30 :: Pixel8
        v = fromIntegral y*30 :: Pixel8

pixelung16 :: Int -> Int -> PixelRGB16
pixelung16 x y = PixelRGB16 u v (u*v)
    where
        u = fromIntegral x :: Pixel16
        v = fromIntegral y :: Pixel16

pic :: Image PixelRGB8
pic = generateImage pixelung 5 5 

dynpic :: DynamicImage
dynpic = ImageRGB8 (pic)




saveImage :: FilePath -> DynamicImage -> IO ()
saveImage name img  = do
    savePngImage (name ++ ".png") img
    putStrLn "Image saved." 

loadPng :: FilePath -> IO DynamicImage
loadPng path = do
    temp <- readPng path >>= either error return
    return temp






