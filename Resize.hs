module Resize where

import Dither 
import qualified Data.Vector.Storable.Mutable as M
import Codec.Picture
import Data.Word
import Control.Monad( forM_, foldM, liftM, ap )
import Control.DeepSeq( NFData( .. ) )
import Control.Monad.ST( runST )
import Control.Monad.Primitive ( PrimMonad, PrimState )
import Foreign.Storable ( Storable )
import Data.Bits( unsafeShiftL, unsafeShiftR, shiftR )
import Data.Word( Word8, Word16 )
import Data.List( foldl' )
import Data.Vector.Storable ( (!) )
import qualified Data.Vector.Storable as V
import Data.Ratio
import Control.Arrow
import Control.Monad
import Control.Applicative
import System.FilePath
import System.Directory

-- DIN A4 210 x 297 mm
-- DIN A5 148 x 210 mm
-- DIN A6 105 x 148 mm
-- DIN A7 74 x 105 mm


type Accessor = PixelRGB8 -> Pixel8
type DrawMax = (Int,Int)
type ImgMax = (Int,Int)
-- Getters for pixel components, as the constructor does not
-- provide any public ones.
red, blue, green :: Accessor
red   (PixelRGB8 r _ _) = r
green (PixelRGB8 _ g _) = g
blue  (PixelRGB8 _ _ b) = b

getImgSize :: FilePath -> IO (Maybe (Int,Int))
getImgSize pIn = do b <- readImage pIn >>= either error return
                    return $ f b
    where f (ImageRGB8 i) = Just (imageWidth i, imageHeight i)
          f _             = Nothing


-- Source: https://gist.github.com/eflister/5456125
goResize :: FilePath -> FilePath -> Int -> IO ()
goResize inF outF h = either error f =<< readImage inF
    where f (ImageRGB8 i) = if fact < 1 
                                then do putStrLn $ "making " ++ outF
                                        savePngImage outF . ImageRGB8 $ resize fact i   
                                else error "only shrinks"
                            where fact = h % (imageHeight i)
          f _             = error "only does ImageRGB8"          
 
--i hate that this is png/PixelRGB8 specific, but see https://github.com/Twinside/Juicy.Pixels/issues/1
--resize :: (RealFrac a, Pixel b) => a -> Image b -> Image b
resize :: (RealFrac a) => a -> Image PixelRGB8 -> Image PixelRGB8
resize fact i = uncurry (generateImage f) new
    where f = curry $ (pixelAt' old (round $ 1/fact) i) . (uncurry (***) $ (join (***) tmp) (fst,snd))
          old = (imageWidth &&& imageHeight) i
          new = (join (***) $ scale fact) old
          scale r = round . (* (toRational r)) . toRational
          tmp s = scale (s old) . (% (s new))
 
-- pretty slow, should use repa or something
pixelAt' :: (Int, Int) -> Int -> Image PixelRGB8 -> (Int, Int) -> PixelRGB8
pixelAt' (dw,dh) s i (x,y) = avg pix
    where inds n d = [a | a <- (+ n) <$> [0..s], all id ([(>= 0), (< d)] <*> [a])]
          pix = (uncurry $ pixelAt i) <$> [(x',y') | x' <- inds x dw, y' <- inds y dh]
          avg p = (foldl pp (0,0,0) p) `pd` (length p)
          pp (r, g, b) (PixelRGB8 r' g' b') = (pf r r', pf g g', pf b b')
          pf a = (+ a) . fromIntegral
          pd (r, g, b) d = PixelRGB8 (pr r d) (pr g d) (pr b d)
          pr a b = round (a % b)


getConF :: (Int,Int) -> DrawMax -> Int
getConF (imgX,imgY) (drawX,drawY)
    | imgX >= imgY   = truncate $ fromIntegral $ imgX `div` drawX
    | otherwise      = truncate $ fromIntegral $ imgY `div` drawX


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





-- -- Resizing Image by calculating average pixel Values
-- resizeImage :: DrawMax -> Image PixelRGB8 -> Image PixelRGB8
-- resizeImage (dw,dh) img@(Image { imageWidth  = w, 
--                                  imageHeight = h, 
--                                  imageData   = vec }) =
--   Image dw dh pixels
--     where compCount = 3
--           conF = getConF (w,h) (dw,dh)
--           pixels = runST $ do
--             newArr <- M.new (w * h * compCount)
--             let lineMapper _ _ y | y >= h = return ()
--                 lineMapper readIdxLine writeIdxLine y = colMapper readIdxLine writeIdxLine 0
--                   where colMapper readIdx writeIdx x
--                             | x >= w = lineMapper readIdx writeIdx $ y + 1*(conF-1)
--                             | otherwise = do
--                                 unsafeWritePixel newArr writeIdx $ calcAveragePix
--                                 colMapper (readIdx  + compCount*(conF-1)) (writeIdx + compCount*(conF-1)) (x + 1*(conF-1))
--                                     where
--                                     -- base of pixel of this instant point (x,y)
--                                     baseA :: Int
--                                     baseA = (x + y * w) * compCount
--                                     -- calculates average pixel from here
--                                     calcAveragePix = average $ arrPix (x,y)
--                                     -- calculates a list of all pixels in this resize-square
--                                     arrPix :: Point ->[PixelRGB8]
--                                     arrPix p = pToPix $ imgToPoints p (conF-1) (w,h)
--                                         where
--                                         -- Point -> conF -> ImgMax -> [Point]
--                                         imgToPoints :: Point -> Int -> ImgMax -> [Point]
--                                         imgToPoints (x,y) f (xmax,ymax) = [(x',y')| 
--                                                                                 x' <- [x..(x+f)], 
--                                                                                 y' <- [y..(y+f)], 
--                                                                                 x' <= (xmax-1),
--                                                                                 y' <= (ymax-1)]
--                                         pToPix :: [Point] -> [PixelRGB8]
--                                         pToPix []     = []
--                                         pToPix (p:ps) = (uncurry (pixelAt img) p): pToPix ps 
--             lineMapper 0 0 0

--             -- unsafeFreeze avoids making a second copy and it will be
--             -- safe because newArray can't be referenced as a mutable array
--             -- outside of this where block
--             V.unsafeFreeze newArr













