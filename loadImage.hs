{-# LANGUAGE TypeFamilies #-}


module LoadImage where

import MakeIMG
import Dither
import Resize

import Codec.Picture.Types 
import System.Environment
import Data.Bits( unsafeShiftR )
import Data.Word( Word8, Word16 )
import qualified Data.Vector.Storable as V


-- DynamicImage to Image PixelRGB8 converter
dyn2rgb8 :: DynamicImage -> Maybe (Image PixelRGB8)
dyn2rgb8 (ImageRGB8   img) = Just $ img
dyn2rgb8 (ImageY8     img) = Just $ promoteImage img
dyn2rgb8 (ImageY16    img) = dyn2rgb8 $ ImageRGB16 $ promoteImage img
dyn2rgb8 (ImageYA8    img) = Just $ promoteImage $ pixelMap dropTransparency img
dyn2rgb8 (ImageYA16   img) = dyn2rgb8 $ ImageY16 $ pixelMap dropTransparency img
dyn2rgb8 (ImageRGBA8  img) = Just $ pixelMap dropTransparency img
dyn2rgb8 (ImageRGBA16 img) = dyn2rgb8 $ ImageRGB16 $ pixelMap dropTransparency img
dyn2rgb8 (ImageYCbCr8 img) = Just $ convertImage img
dyn2rgb8 (ImageCMYK8  img) = Just $ convertImage img
dyn2rgb8 (ImageRGB16  img) = Just $ from16to8' img
  where
  -- Source: http://hackage.haskell.org/package/JuicyPixels-3.2.2/docs/src/Codec-Picture-Saving.html
  from16to8' :: ( PixelBaseComponent source ~ Word16
                , PixelBaseComponent dest   ~ Word8 )
            => Image source -> Image dest
  from16to8' Image { imageWidth = w, imageHeight = h
                  , imageData = arr } = Image w h transformed
     where transformed = V.map toWord8 arr
           toWord8 v = fromIntegral (v `unsafeShiftR` 8)
dyn2rgb8 (ImageCMYK16 img) = dyn2rgb8 $ ImageRGB16 $ convertImage img
dyn2rgb8 (ImageRGBF   img) = Just $ pixelMap pFtoRGB8 img
  where
  -- Source: http://hackage.haskell.org/package/JuicyPixels-3.1.5/docs/src/Codec-Picture-ColorQuant.html
  pFtoRGB8 :: PixelRGBF -> PixelRGB8
  pFtoRGB8 (PixelRGBF r g b) =
    PixelRGB8 (round r) (round g) (round b)
--dyn2rgb8 (ImageYF     img) = undefined
dyn2rgb8 _                 = Nothing


-- for Debugging
dyn2string :: DynamicImage -> IO()
dyn2string (ImageY8 d)       = putStrLn "ImageY8"
dyn2string (ImageY16 d)      = putStrLn "ImageY16"
dyn2string (ImageYF d)       = putStrLn "ImageYF"
dyn2string (ImageYA8 d)      = putStrLn "ImageYA8"
dyn2string (ImageYA16 d)     = putStrLn "ImageYA16"
dyn2string (ImageRGB8 d)     = putStrLn "ImageRGB8"
dyn2string (ImageRGB16 d)    = putStrLn "ImageRGB16"
dyn2string (ImageRGBF d)     = putStrLn "ImageRGBF"
dyn2string (ImageRGBA8 d)    = putStrLn "ImageRGBA8"
dyn2string (ImageRGBA16 d)   = putStrLn "ImageRGBA16"
dyn2string (ImageYCbCr8 d)   = putStrLn "ImageYCbCr8"
dyn2string (ImageCMYK8 d)    = putStrLn "ImageCMYK8"
dyn2string (ImageCMYK16 d)   = putStrLn "ImageCMYK16"


-- main :: IO ()
-- main = do
--     putStrLn "Hallo zu LambDraw!"
--     args <- getArgs
--     checkArgs args

-- main1 :: [String] -> IO()
-- main1 args = do
--     putStrLn $ show args
--     checkArgs args

-- checkArgs :: [String] -> IO()
-- checkArgs [pathIn,pathOut] = do 
--     putStrLn "Choose Option:"
--     putStrLn "1 rgbPallette  Floyd-Steinberg Dither"
--     putStrLn "2 cmykPallette Floyd-Steinberg Dither"
--     putStrLn "3 noDither conversion"  
--     putStrLn "4 resize"
--     opt <- getLine
--     checkopt $ (read opt :: Int)
--     processImage (read opt :: Int) pathIn pathOut
--     where 
--     checkopt :: Int -> IO()
--     checkopt 1  = putStrLn "Starting rgb  conversion."    
--     checkopt 2  = putStrLn "Starting cmyk conversion"     
--     checkopt 3  = putStrLn "Starting noDither conversion" 
--     checkopt _ = do 
--       putStrLn "Wrong Input"
--       checkArgs [pathIn,pathOut] 
-- checkArgs _                = hilfstext
--     where 
--     hilfstext :: IO ()
--     hilfstext = do
--       putStrLn "Input needed:"
--       putStrLn "<pathIn.png> <pathOut>"
--       args'  <- getLine
--       checkArgs $ words args'


processImage :: Int -> FilePath -> FilePath -> IO()
processImage opt pathIn pathOut = do
    dynImg <- loadPng pathIn
    dyn2string dynImg -- Debugging Helper
    safesave pathOut $ (dyn2rgb8 dynImg)
        where 
        safesave pOut (Just img) = saveImage pathOut $ ImageRGB8 $ imgConverter opt img
        safesave _    Nothing    = putStrLn "Error with Image to RGB8 conversion!"

imgConverter :: Int -> Image PixelRGB8 -> Image PixelRGB8
imgConverter state img@( Image {  imageWidth  = w
                                , imageHeight = h
                                , imageData   = arr })
    | state == 1 = ditherFloydRGB8 rgbPixls img  
    | state == 2 = ditherFloydRGB8 cmykPixls img  
    | state == 3 = pixelMap (noDither cmykPixls) img
--    | state == 4 = resizeImage rgbPixls img   
    | otherwise  = pixelMap (noDither rgbPixls)  img  

-- imgResizer :: Int 

processImagels :: [PixelRGB8] -> FilePath -> FilePath -> IO()
processImagels pixls pathIn pathOut = do
    dynImg <- loadPng pathIn
    dyn2string dynImg -- Debugging Helper
    safesave pathOut $ (dyn2rgb8 dynImg)
        where 
        safesave pOut (Just img) = saveImage pathOut $ ImageRGB8 $ ditherFloydRGB8 pixls img
        safesave _    Nothing    = putStrLn "Error with Image to RGB8 conversion!"










