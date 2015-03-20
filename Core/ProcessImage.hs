{-# LANGUAGE TypeFamilies #-}


module Core.ProcessImage where

import Core.MakeIMG
import Core.Dither
import Core.Resize
import Core.Colorsplicer
import Data.List.Utils as L
import Codec.Picture
import Codec.Picture.Types 
import System.IO
import System.Environment
import Control.Monad
import Control.Parallel.Strategies
import Control.DeepSeq
import Data.Word( Word8, Word16 )
import Data.Ratio
import System.Directory

-------------------------------------------------------------------------------
----            Image Processing
-------------------------------------------------------------------------------
preProcessImage :: (Image PixelRGB8 -> [PixelRGB8] -> FilePath -> IO()) 
                -> [PixelRGB8] -> FilePath -> FilePath -> IO()
preProcessImage fun pixls pathIn pathOut = do
    createDirectoryIfMissing False "./images/temp/"
    dynImg <- loadPng pathIn
    dyn2string dynImg -- Debugging Helper
    mayImg <- return $ dyn2rgb8 dynImg
    if (isNothing mayImg)
      then do print "Error with loading PNG"
      else do
        (Just img) <- return mayImg
        fun img pixls pathOut 
        putStrLn "Image Processed."
     where isNothing Nothing = True
           isNothing _       = False

doResize :: Int -> Image PixelRGB8 -> [PixelRGB8] -> FilePath -> IO ()
doResize hnew img _ pathOut = do let fact = hnew % (imageHeight img)
                                 saveImage "Resize done" (pathOut++"_res") $ ImageRGB8 $ resize fact img

doTestDither :: Image PixelRGB8 -> [PixelRGB8] -> FilePath -> IO ()
doTestDither img pixls pathOut = do dithImg    <- processDither pixls img pathOut
                                    putStrLn "Test Done."


processImage :: [PixelRGB8] -> FilePath -> FilePath -> IO()
processImage pixls pathIn pathOut = do
    createDirectoryIfMissing False "./images/temp/"
    let checkFile = doesFileExist pathIn 
    dynImg <- loadPng pathIn
    dyn2string dynImg -- Debugging Helper
    mayImg <- return $ dyn2rgb8 dynImg
    if (isNothing mayImg)
      then do print "Error with loading PNG"
      else do
        (Just img) <- return mayImg
        dithImg    <- processDither pixls img pathOut
        splicelss  <- processSplice pixls dithImg pathOut
        processToolpath img splicelss pathOut
        putStrLn "Image Processed."
     where isNothing Nothing = True
           isNothing _       = False

processDither :: [PixelRGB8] -> Image PixelRGB8 -> FilePath -> IO(Image PixelRGB8)
processDither pal img pathOut = do
    dimg <- return $ ditherFloydRGB8 pal img
    saveImage "Dither processed" (pathOut++"_dith") $ ImageRGB8 dimg
    return dimg


processSplice :: [PixelRGB8] -> Image PixelRGB8 -> FilePath -> IO([(Int,[Point])])
processSplice pal img@(Image { imageWidth  = w, 
                                 imageHeight = h}) pathOut = 
  do  splicelss  <- return $ runEval $ do parMap1 ((starSort w h) . (colorSplicer img)) pal
      let tupsplices = zip [0..(length splicelss)] splicelss
      mapM_ (spliceSave pathOut) tupsplices
      --return $ runEval $ do parMap1 (spliceSave pathOut) tupsplices
      return tupsplices
        where
        spliceSave pOut (nr,[])   = putStrLn $ "For white ("++show nr++") no splicing needed. No file created."
        spliceSave pOut (nr,spls) = saveImage ("Splicing "++show nr++" done") (pOut++"_splice"++show nr) 
                                          $ ImageRGB8 $ pointlsToImg spls w h

processToolpath :: Image PixelRGB8 -> [(Int,[Point])] -> FilePath -> IO()
processToolpath im [] _   = return () 
processToolpath (Image { imageWidth  = w, 
                         imageHeight = h}) pss pOut  = do 
          gcodes   <- return $ runEval $ do parMap1 toGcode pss
          codeLstoFile (pOut++"_gcode.txt") $ concat gcodes
-------------------------------------------------------------------------------
----            Generate GCode
-------------------------------------------------------------------------------
-- spliceToGCode :: FilePath -> [PixelRGB8] -> IO()
-- spliceToGCode pathIn pixls = 
--   runEval $ do let pathOut = L.replace ".png" "_gcode.txt" pathIn

--                lsToGcode pathOut $ []

codeLstoFile :: FilePath -> [String] -> IO()
codeLstoFile _   []  = print "Error with writing TextFile."
codeLstoFile url ls  = do outh <- openFile url WriteMode
                          mapM_ (hPutStrLn outh) ls 
                          hClose outh


-- lsToGcode :: FilePath -> [Point] -> IO()
-- lsToGcode url ls = do outh <- openFile url WriteMode
--                       mapM_ (hPutStrLn outh) $ (toGcode ls) 
--                       hClose outh

toGcode :: (Int,[Point]) -> [String]
toGcode (_,[]) = ["G00 X00 Y00"] -- Jog Home
toGcode (nr,(p:ps)) = setPen : toGString p : "M08" : "G04 P800" : "M09" : "G04 P100" : toGcode' ps
  where toGcode' []     = ["G00 X00 Y00"]
        toGcode' (p:ps) = toGString p : "M08" : "G04 P800" : "M09" : "G04 P100" : toGcode' ps
        toGString (px,py) = "G00 X" ++ show px ++ " Y" ++ show py
        setPen  = "Placeholder for PenChoosing: "++show nr -- needs to be adressed in Hardware first
        --dropPen = "M08" : "G04 P800" : "M09" : "G04 P100"




