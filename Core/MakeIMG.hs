module Core.MakeIMG where

import Codec.Picture
import Codec.Picture.Types
import Data.Word( Word8, Word16 )
import Control.Parallel.Strategies

-- Type & Data declarations
type Point      = (Int,Int)
type Distance   = Float
data PixError   = PixError Int Int Int 
    deriving (Show,Eq,Ord)
type Accessor = PixelRGB8 -> Pixel8

--type BasID      = Int
--type ErrorFac   = Int

-- Getters for pixel components, as the constructor does not
-- provide any public ones.
red, blue, green :: Accessor
red   (PixelRGB8 r _ _) = r
green (PixelRGB8 _ g _) = g
blue  (PixelRGB8 _ _ b) = b

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

picW = 100

-- ====== IMAGESIZE <---------------<--------
pic :: Image PixelRGB8
pic = generateImage pixelung picW picW 

dynpic :: DynamicImage
dynpic = ImageRGB8 (pic)

pixelung :: Int -> Int -> PixelRGB8
pixelung x y = PixelRGB8 u v (u*v)
    where
        u = fromIntegral x*10 :: Pixel8
        v = fromIntegral y*10 :: Pixel8

pixelungGrad :: Int -> Int -> PixelRGB8
pixelungGrad x _ = PixelRGB8 255 g 0
  where g = fromIntegral x :: Pixel8

pixelung16 :: Int -> Int -> PixelRGB16
pixelung16 x y = PixelRGB16 u v (u*v)
    where
        u = fromIntegral x :: Pixel16
        v = fromIntegral y :: Pixel16

pixelungls :: [Point] -> Int -> Int -> PixelRGB8
pixelungls ls x y
    | pointInList ls (x,y) = magentaPix
    | otherwise = whitePix
        where pointInList :: [Point] -> Point -> Bool
              pointInList [] _ = False
              pointInList ((px,py):pts) (x,y)
                | and [x==px,y==py] = True
                | otherwise = pointInList pts (x,y)

saveImage :: String -> FilePath -> DynamicImage -> IO ()
saveImage str name img  = do
    savePngImage (name ++ ".png") img
    putStrLn $ str++" and Image saved." 

loadPng :: FilePath -> IO DynamicImage
loadPng path = do
    temp <- readPng path >>= either error return
    return temp

---- Pixel Functions 


-- Perform a componentwise pixel operation.
compwise :: (Word8 -> Word8 -> Word8) -> PixelRGB8 -> PixelRGB8 -> PixelRGB8
compwise f (PixelRGB8 ra ga ba) (PixelRGB8 rb gb bb) =
  PixelRGB8 (f ra rb) (f ga gb) (f ba bb)

-- Compute the absolute difference of two pixels.
diffPixel :: PixelRGB8 -> PixelRGB8 -> PixelRGB8
diffPixel = compwise (\x y -> max x y - min x y)


-- Compute the average value of a list of pixels.
average :: [PixelRGB8] -> PixelRGB8
average pixels = PixelRGB8 (avg red) (avg green) (avg blue)
  where
    len   = toInteger $ length pixels
    avg c = fromIntegral $ (sum $ map (toInteger . c) pixels) `div` len

-- Compute the Euclidean distance squared between two pixels.
distPixel :: PixelRGB8 -> PixelRGB8 -> Integer
distPixel x y = (rr ^ 2) + (gg ^ 2) + (bb ^ 2)
  where
    PixelRGB8 r g b = diffPixel x y
    rr              = toInteger r
    gg              = toInteger g
    bb              = toInteger b

parMap1 :: (a -> b) -> [a] -> Eval [b]
parMap1 f [] = return []
parMap1 f (a:as) = do
   b <- rpar (f a)
   bs <- parMap1 f as
   return (b:bs)



