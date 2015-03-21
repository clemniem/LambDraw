{-# LANGUAGE TypeFamilies #-}


module Core.ProcessImage where

import Core.MakeIMG
import Core.Dither
import Core.Resize
import Core.Colorsplicer

import Codec.Picture
import System.IO
import Control.Parallel.Strategies
import Data.Ratio
import System.Directory

-------------------------------------------------------------------------------
----            Image Processing
-------------------------------------------------------------------------------
-- | Preprocessing is for intermediate steps (resize,testDither) 
preProcessImage :: (Image PixelRGB8 -> [PixelRGB8] -> FilePath -> IO()) 
                -> [PixelRGB8] -> FilePath -> FilePath -> IO()
preProcessImage fun pixls pathIn pathOut = do
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

-- | Does the Resize
doResize :: Int -> Image PixelRGB8 -> [PixelRGB8] -> FilePath -> IO ()
doResize hnew img _ pathOut = do let fact = hnew % (imageHeight img)
                                 putStrLn $ "Fact Resize: "++show fact
                                 if (fact == 1%1) 
                                  then saveImage "No Resize done ratio == 1%1" (pathOut++"_res") $ ImageRGB8 $ img
                                  else saveImage "Resize done" (pathOut++"_res") $ ImageRGB8 $ resize fact img
-- | Does the Test Dither 
doTestDither :: Image PixelRGB8 -> [PixelRGB8] -> FilePath -> IO ()
doTestDither img pixls pathOut = do _dithImg    <- doDither pixls img pathOut
                                    print "Test Done."

-- | Processes the Image => Dither and Splice
processImage :: [PixelRGB8] -> FilePath -> FilePath -> IO()
processImage pixls pathIn pathOut = do
    createDirectoryIfMissing False "./images/temp/"
    dynImg <- loadPng pathIn
    dyn2string dynImg -- Debugging Helper
    mayImg <- return $ dyn2rgb8 dynImg
    if (isNothing mayImg)
      then do print "Error with loading PNG"
      else do
        (Just img) <- return mayImg
        dithImg    <- doDither pixls img pathOut
        splicelss  <- doSplice pixls dithImg pathOut
        processToolpath img splicelss pathOut
        putStrLn "Image Processed."
     where isNothing Nothing = True
           isNothing _       = False

-- | Does the Dither
doDither :: [PixelRGB8] -> Image PixelRGB8 -> FilePath -> IO(Image PixelRGB8)
doDither pal img pathOut = do
    dimg <- return $ ditherFloydRGB8 pal img
    saveImage "Dither processed" (pathOut++"_dith") $ ImageRGB8 dimg
    return dimg

-- | Does the Splice
-- Takes a Pallette and an Image and creates a list of Tuples (Int,[Point])
-- Int     ^= Number of the Color in Pallette 
-- [Point] ^= List of Points for that Color
doSplice :: [PixelRGB8] -> Image PixelRGB8 -> FilePath -> IO([(Int,[Point])])
doSplice pal img@(Image { imageWidth  = w, 
                                 imageHeight = h}) pathOut = 
  do  splicelss  <- return $ runEval $ do parMap1 ((starSort w h) . (colorSplicer img)) pal
      let tupsplices = zip [0..(length splicelss)] splicelss
      mapM_ (spliceSave pathOut) tupsplices
      return tupsplices
        where
        spliceSave _pOut (nr,[])   = putStrLn $ "For white ("++show nr++") no splicing needed. No file created."
        spliceSave pOut (nr,spls)  = saveImage ("Splicing "++show nr++" done") (pOut++"_splice"++show nr) 
                                          $ ImageRGB8 $ pointlsToImg spls w h

-- | generates the Toolpath from an Image (work in Progress...)
processToolpath :: Image PixelRGB8 -> [(Int,[Point])] -> FilePath -> IO()
processToolpath _im [] _   = return () 
processToolpath (Image { imageWidth  = w, 
                         imageHeight = h}) pss pOut  = do 
          gcodes   <- return $ runEval $ do parMap1 toGcode pss
          codeLstoFile (pOut++"_gcode.txt") $ concat gcodes
-------------------------------------------------------------------------------
----            Generate GCode
-------------------------------------------------------------------------------

-- | Writes the File
codeLstoFile :: FilePath -> [String] -> IO()
codeLstoFile _   []  = print "Error with writing TextFile."
codeLstoFile url ls  = do outh <- openFile url WriteMode
                          mapM_ (hPutStrLn outh) ls 
                          hClose outh

-- | Generates GCode
toGcode :: (Int,[Point]) -> [String]
toGcode (_,[]) = ["G00 X00 Y00"] -- Jog Home
toGcode (nr,(p:ps)) = setPen : toGString p : "M08" : "G04 P800" : "M09" : "G04 P100" : toGcode' ps
  where toGcode'  []       = ["G00 X00 Y00"]
        toGcode'  (p':ps') = toGString p' : "M08" : "G04 P800" : "M09" : "G04 P100" : toGcode' ps'
        toGString (px,py)  = "G00 X" ++ show (px+fst offset) ++ " Y" ++ show (py +snd offset)
        [offset] = take 1  $ drop (nr-1) [(0,0),(5,0),(0,5),(5,5)] 
        setPen  = "Placeholder for PenChoosing: "++show nr -- needs to be adressed in Hardware first
        --dropPen = "M08" : "G04 P800" : "M09" : "G04 P100"




