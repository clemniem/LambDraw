
module Colorsplicer where

import Dither
import           Data.Vector          (Vector, (!))
import qualified Data.Vector.Storable as VS
import           Codec.Picture.Types
import Control.Monad( foldM, liftM, ap )
import Control.DeepSeq( NFData( .. ) )
import Control.Monad.ST as ST
import Control.Monad.Primitive ( PrimMonad, PrimState )
import Foreign.ForeignPtr( castForeignPtr )
import Foreign.Storable ( Storable )
import Data.Bits( unsafeShiftL, unsafeShiftR, (.|.), (.&.),shiftR )
import Data.Word( Word8, Word16, Word32, Word64 )
import Data.List( foldl' )
import Data.Vector.Storable ( (!) )
import qualified Data.Vector.Storable.Mutable as M
import Data.Maybe

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

dynDith = ImageRGB8 picDith

picDith = ditherFloydRGB8 rgbPixls pic


saveImage :: FilePath -> DynamicImage -> IO ()
saveImage name img  = do
    savePngImage (name ++ ".png") img
    putStrLn "Gespeichert." 

loadPng :: FilePath -> IO DynamicImage
loadPng path = do
    temp <- readPng path >>= either error return
    return temp

checkColor :: PixelRGB8 -> PixelRGB8 -> Bool
checkColor (PixelRGB8 a1 a2 a3) (PixelRGB8 b1 b2 b3)
    | a1 /= b1 = False
    | a2 /= b2 = False 
    | a3 /= b3 = False
    | otherwise = True

colorSplicer :: Image PixelRGB8 -> PixelRGB8 -> [Point] 
colorSplicer img@(Image { imageWidth  = w, 
                          imageHeight = h, 
                          imageData   = vec }) pix =
    [(x,y)| x <- [0..w-1], y <-[0..h-1], checkColor (pixelAt img x y) pix]

































