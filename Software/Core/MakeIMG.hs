{-# LANGUAGE TypeFamilies #-}

module Core.MakeIMG where

import Codec.Picture
import Codec.Picture.Types
import Control.Parallel.Strategies
import Data.Bits( unsafeShiftR )
import qualified Data.Vector.Storable as V
import Data.Word( Word8, Word16 )


-- Type & Data declarations
type Point      = (Int,Int)
type Distance   = Float
data PixError   = PixError Int Int Int 
    deriving (Show,Eq,Ord)
type Accessor = PixelRGB8 -> Pixel8

--type BasID      = Int
--type ErrorFac   = Int

-- | Getters for pixel components, as the constructor does not
-- provide any public ones.
red, blue, green :: Accessor
red   (PixelRGB8 r _ _) = r
green (PixelRGB8 _ g _) = g
blue  (PixelRGB8 _ _ b) = b

-- | HelperVariables for Testing
redPix,greenPix,bluePix,blackPix,whitePix,yellowPix,magentaPix,cyanPix :: PixelRGB8
redPix     = PixelRGB8 255 0   0
greenPix   = PixelRGB8 0   255 0
bluePix    = PixelRGB8 0   0   255
blackPix   = PixelRGB8 0   0   0
whitePix   = PixelRGB8 255 255 255
yellowPix  = PixelRGB8 255 255 0
magentaPix = PixelRGB8 255 0   255
cyanPix    = PixelRGB8 0   255 255

-- | PixelLists for rgb and cmyk
rgbPixls,cmykPixls :: [PixelRGB8]
rgbPixls   = [redPix,greenPix,bluePix,blackPix,whitePix]
cmykPixls  = [cyanPix,magentaPix,yellowPix,blackPix,whitePix]

-- | Helper Set Size of TestImage
picW :: Int
picW = 100

-- ====== IMAGESIZE <---------------<--------
-- | Image PixelRGB8 for debugging
pic :: Image PixelRGB8
pic = generateImage pixelung picW picW 

-- | DynamicImage for debugging
dynpic :: DynamicImage
dynpic = ImageRGB8 (pic)

-- | Pixel generator: LambDraw
pixelung :: Int -> Int -> PixelRGB8
pixelung x y = PixelRGB8 u v (u*v)
    where
        u = fromIntegral x*10 :: Pixel8
        v = fromIntegral y*10 :: Pixel8

-- | Pixel generator: Gradient
pixelungGrad :: Int -> Int -> PixelRGB8
pixelungGrad x _ = PixelRGB8 255 g 0
  where g = fromIntegral x :: Pixel8

-- | Pixel generator : 
pixelung16 :: Int -> Int -> PixelRGB16
pixelung16 x y = PixelRGB16 u v (u*v)
    where
        u = fromIntegral x :: Pixel16
        v = fromIntegral y :: Pixel16

-- | Pixel generator: from List
pixelungls :: PixelRGB8 -> [Point] -> Int -> Int -> PixelRGB8
pixelungls (PixelRGB8 255 255 255) _ls _x _y = whitePix
pixelungls col ls x y
    | elem (x,y) ls = col
    | otherwise     = whitePix


-------------------------------------------------------------------------------
----            Image Loading
-------------------------------------------------------------------------------
-- | DynamicImage to Image PixelRGB8 converter
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

-- | Helper for Debugging
dyn2string :: DynamicImage -> IO()
dyn2string (ImageY8     _)   = putStrLn "ImageY8 loading..."
dyn2string (ImageY16    _)   = putStrLn "ImageY16 loading..."
dyn2string (ImageYF     _)   = putStrLn "ImageYF loading..."
dyn2string (ImageYA8    _)   = putStrLn "ImageYA8 loading..."
dyn2string (ImageYA16   _)   = putStrLn "ImageYA16 loading..."
dyn2string (ImageRGB8   _)   = putStrLn "ImageRGB8 loading..."
dyn2string (ImageRGB16  _)   = putStrLn "ImageRGB16 loading..."
dyn2string (ImageRGBF   _)   = putStrLn "ImageRGBF loading..."
dyn2string (ImageRGBA8  _)   = putStrLn "ImageRGBA8 loading..."
dyn2string (ImageRGBA16 _)   = putStrLn "ImageRGBA16 loading..."
dyn2string (ImageYCbCr8 _)   = putStrLn "ImageYCbCr8 loading..."
dyn2string (ImageCMYK8  _)   = putStrLn "ImageCMYK8 loading..."
dyn2string (ImageCMYK16 _)   = putStrLn "ImageCMYK16 loading..."

-- | getter for Image size
getImgSize :: FilePath -> IO (Maybe (Int,Int))
getImgSize pIn = do dyn <- readPng pIn >>= either error return
                    return $ getSize $ dyn2rgb8 dyn
    where getSize (Just img) = Just (imageWidth img, imageHeight img)
          getSize _          = Nothing

-- | Helper Function to Load an Image and prompt report
loadPng :: FilePath -> IO DynamicImage
loadPng path = do
    temp <- readPng path >>= either error return
    return temp

-- | Helper Function to Save an Image and prompt report
saveImage :: String -> FilePath -> DynamicImage -> IO ()
saveImage str name img  = do
    savePngImage (name ++ ".png") img
    putStrLn $ str++" and Image saved." 

-- | DEBUGGING Helper to save Test-Images
saveImg::Image PixelRGB8 -> IO ()
saveImg i = saveImage "Testing" "./images/test.png" $ ImageRGB8 i
-------------------------------------------------------------------------------
----            Pixel Functions
-------------------------------------------------------------------------------

-- | Perform a componentwise pixel operation.
compwise :: (Word8 -> Word8 -> Word8) -> PixelRGB8 -> PixelRGB8 -> PixelRGB8
compwise f (PixelRGB8 ra ga ba) (PixelRGB8 rb gb bb) =
  PixelRGB8 (f ra rb) (f ga gb) (f ba bb)

-- | Compute the absolute difference of two pixels.
diffPixel :: PixelRGB8 -> PixelRGB8 -> PixelRGB8
diffPixel = compwise (\x y -> max x y - min x y)

-- | Compute the average value of a list of pixels.
average :: [PixelRGB8] -> PixelRGB8
average pixels = PixelRGB8 (avg red) (avg green) (avg blue)
  where
    len   = toInteger $ length pixels
    avg c = fromIntegral $ (sum $ map (toInteger . c) pixels) `div` len

-- Compute the Euclidean distance squared between two pixels.
distPixel :: PixelRGB8 -> PixelRGB8 -> Integer
distPixel x y = fromIntegral $ foldl (flip (^)) 2 [r,g,b]
  where
    (PixelRGB8 r g b) = diffPixel x y

-------------------------------------------------------------------------------
----            Misc Util Functions
-------------------------------------------------------------------------------

-- | operate a Function on Tuples, elementwise
funTupel :: (t -> t1 -> t2) -> (t, t) -> (t1, t1) -> (t2, t2)
funTupel f (x1,x2) (y1,y2) = (f x1 y1,f x2 y2)

-- | parMap implementation with rpar
parMap1 :: (a -> b) -> [a] -> Eval [b]
parMap1 _ [] = return []
parMap1 f (a:as) = do
   b <- rpar (f a)
   bs <- parMap1 f as
   return (b:bs)




