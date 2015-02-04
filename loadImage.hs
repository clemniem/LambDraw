{-# LANGUAGE TypeFamilies #-}

--module LoadImage where

import Dither
import Control.Applicative
import Control.Monad
import Codec.Picture 
import Codec.Picture.Types 
import Data.Ratio (Ratio, (%))
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Storable (pokeByteOff)
import System.Environment
import Data.Bits( unsafeShiftR )
import Data.Word( Word8, Word16 )
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector.Storable as V


main :: IO ()
main = do
    putStrLn "Hallo zu LambDraw!"
    args <- getArgs
    checkArgs args

checkArgs :: [String] -> IO()
checkArgs [pathIn,pathOut] = processImage pathIn pathOut
checkArgs [pathIn] = processImage pathIn "processed" 
checkArgs _ = hilfstext
    where 
    hilfstext :: IO ()
    hilfstext = do
      putStrLn "Eingabeformat ist:"
      putStrLn "<pathIn> <pathOut>"

processImage :: FilePath -> FilePath -> IO()
processImage pathIn pathOut = do
    dynImg <- loadPng pathIn
    dyn2string dynImg
    safesave pathOut $ (dyn2rgb8 dynImg)
        where 
        safesave pOut (Just img) = saveImage pathOut $ ImageRGB8 $ imgConverter 1 img
        safesave _    Nothing    = putStrLn "Fehler bei Bildumwandlung!Falsches Bildformat!"

loadPng :: FilePath -> IO DynamicImage
loadPng path = do
    temp <- readPng path >>= either error return
    return temp

saveImage :: FilePath -> DynamicImage -> IO ()
saveImage name img  = do 
    savePngImage (name ++ ".png") img
    putStrLn "Gespeichert." 

imgConverter :: Int -> Image PixelRGB8 -> Image PixelRGB8
imgConverter state img@( Image {  imageWidth  = w
                                , imageHeight = h
                                , imageData   = arr })
    | state == 1 = ditherFloydRGB8 cmykPix img  
    | state == 2 = pixelMapXY dither img


imgPls w h = [(x,y)| x <- [0..w-1], y <- [0..h-1]]

dyn2rgb8 :: DynamicImage -> Maybe (Image PixelRGB8)
dyn2rgb8 (ImageRGB8   img) = Just $ img
dyn2rgb8 (ImageRGB16  img) = Just $ from16to8 img
dyn2rgb8 (ImageY8     img) = Just $ promoteImage img
dyn2rgb8 (ImageY16    img) = dyn2rgb8 $ ImageRGB16 $ promoteImage img
dyn2rgb8 (ImageYA8    img) = Just $ promoteImage $ pixelMap dropTransparency img
dyn2rgb8 (ImageYA16   img) = dyn2rgb8 $ ImageY16 $ pixelMap dropTransparency img
dyn2rgb8 (ImageRGBA8  img) = Just $ pixelMap dropTransparency img
dyn2rgb8 (ImageRGBA16 img) = dyn2rgb8 $ ImageRGB16 $ pixelMap dropTransparency img
dyn2rgb8 (ImageYCbCr8 img) = Just $ convertImage img
dyn2rgb8 (ImageCMYK8  img) = Just $ convertImage img
dyn2rgb8 (ImageCMYK16 img) = dyn2rgb8 $ ImageRGB16 $ convertImage img
dyn2rgb8 (ImageRGBF   img) = Just $ pixelMap toRGB8 img
--dyn2rgb8 (ImageYF     img) = undefined
dyn2rgb8 _                 = Nothing

-- Source: http://hackage.haskell.org/package/JuicyPixels-3.1.5/docs/src/Codec-Picture-ColorQuant.html
toRGB8 :: PixelRGBF -> PixelRGB8
toRGB8 (PixelRGBF r g b) =
  PixelRGB8 (round r) (round g) (round b)

-- Source: http://hackage.haskell.org/package/JuicyPixels-3.2.2/docs/src/Codec-Picture-Saving.html
from16to8 :: ( PixelBaseComponent source ~ Word16
             , PixelBaseComponent dest ~ Word8 )
          => Image source -> Image dest
from16to8 Image { imageWidth = w, imageHeight = h
                , imageData = arr } = Image w h transformed
   where transformed = V.map toWord8 arr
         toWord8 v = fromIntegral (v `unsafeShiftR` 8)

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













