import Codec.Picture
import Codec.Picture.Types
import Codec.Picture.Png
import Codec.Picture.Saving
import Codec.Picture.ColorQuant
import System.IO
import Data.Word

pixelung :: Int -> Int -> PixelRGB8
pixelung x y = PixelRGB8 u v (u*v)
    where
        u = fromIntegral x :: Pixel8
        v = fromIntegral y :: Pixel8

pixelung16 :: Int -> Int -> PixelRGB16
pixelung16 x y = PixelRGB16 u v (u*v)
    where
        u = fromIntegral x :: Pixel16
        v = fromIntegral y :: Pixel16

pic :: Image PixelRGB8
pic = generateImage pixelung 10 10 

dynpic :: DynamicImage
dynpic = ImageRGB8 (pic)



saveImage :: FilePath -> DynamicImage -> IO ()
saveImage name img  = do
	savePngImage (name ++ ".png") img
	putStrLn "Gespeichert." 

loadPng :: FilePath -> IO DynamicImage
loadPng path = do
    temp <- readPng path >>= either error return
    return temp

-------------------------------------------------------
----------         Main          ----------------------
-------------------------------------------------------







