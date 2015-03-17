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
import Data.Bits( unsafeShiftR )
import Data.Word( Word8, Word16 )
import qualified Data.Vector.Storable as V
import Control.Monad
import Control.Parallel.Strategies
import Control.DeepSeq
-------------------------------------------------------------------------------
----            Image Loading
-------------------------------------------------------------------------------


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


-- Helper for Debugging
dyn2string :: DynamicImage -> IO()
dyn2string (ImageY8 d)       = putStrLn "ImageY8 loading..."
dyn2string (ImageY16 d)      = putStrLn "ImageY16 loading..."
dyn2string (ImageYF d)       = putStrLn "ImageYF loading..."
dyn2string (ImageYA8 d)      = putStrLn "ImageYA8 loading..."
dyn2string (ImageYA16 d)     = putStrLn "ImageYA16 loading..."
dyn2string (ImageRGB8 d)     = putStrLn "ImageRGB8 loading..."
dyn2string (ImageRGB16 d)    = putStrLn "ImageRGB16 loading..."
dyn2string (ImageRGBF d)     = putStrLn "ImageRGBF loading..."
dyn2string (ImageRGBA8 d)    = putStrLn "ImageRGBA8 loading..."
dyn2string (ImageRGBA16 d)   = putStrLn "ImageRGBA16 loading..."
dyn2string (ImageYCbCr8 d)   = putStrLn "ImageYCbCr8 loading..."
dyn2string (ImageCMYK8 d)    = putStrLn "ImageCMYK8 loading..."
dyn2string (ImageCMYK16 d)   = putStrLn "ImageCMYK16 loading..."

getImgSize :: FilePath -> IO (Maybe (Int,Int))
getImgSize pIn = do dyn <- readPng pIn >>= either error return
                    return $ getSize $ dyn2rgb8 dyn
    where getSize (Just img) = Just (imageWidth img, imageHeight img)
          getSize _          = Nothing





parMapf :: (a -> b) -> [a] -> Eval [b]
parMapf f [] = return []
parMapf f (a:as) = do
   b <- rpar (f a)
   bs <- parMap1 f as
   return (b:bs)

-------------------------------------------------------------------------------
----            Image Processing
-------------------------------------------------------------------------------

processImage :: [PixelRGB8] -> FilePath -> FilePath -> IO()
processImage pixls pathIn pathOut = do
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

testDither :: [PixelRGB8] -> FilePath -> FilePath -> IO()
testDither pixls pathIn pathOut = do
    dynImg <- loadPng pathIn
    dyn2string dynImg -- Debugging Helper
    mayImg <- return $ dyn2rgb8 dynImg
    if (isNothing mayImg)
      then do print "Error with loading PNG"
      else do
        (Just img) <- return mayImg
        dithImg    <- processDither pixls img pathOut
        putStrLn "Test Done."
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




