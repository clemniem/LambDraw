import MakeIMG
import Dither
import Resize
import LoadImage

import Codec.Picture.Types 
import System.Environment

main :: IO ()
main = do
    putStrLn "Hallo zu LambDraw!"
    imgRaw <- mainLoadImage
    --mainLandscape
    --imgRes <- mainResize imgRaw
    --imgDith <- mainDither imgRes
    --imgSplice <- mainSplice imgDith
    putStrLn "LambDraw sagt auf Wiedersehen!"

mainLoadImage :: IO ()
mainLoadImage = do
    args <- getArgs
    checkArgs args

checkArgs :: [String] -> IO()
checkArgs [pathIn,pathOut] = do 
    putStrLn pathIn
    putStrLn "Choose Option:"
    putStrLn "1 rgbPallette  Floyd-Steinberg Dither"
    putStrLn "2 cmykPallette Floyd-Steinberg Dither"
    putStrLn "3 noDither conversion"  
    putStrLn "4 resize"
    opt <- getLine
    checkopt $ (read opt :: Int)
    processImage (read opt :: Int) pathIn pathOut
    where 
    checkopt :: Int -> IO()
    checkopt 1  = putStrLn "Starting rgb  conversion."    
    checkopt 2  = putStrLn "Starting cmyk conversion"     
    checkopt 3  = putStrLn "Starting noDither conversion" 
    checkopt _ = do 
      putStrLn "Wrong Input"
      checkArgs [pathIn,pathOut] 
checkArgs _                = hilfstext
    where 
    hilfstext :: IO ()
    hilfstext = do
      putStrLn "Input needed:"
      putStrLn "<pathIn.png> <pathOut>"
      args'  <- getLine
      checkArgs $ words args'


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
