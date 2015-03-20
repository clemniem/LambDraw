module Core.Resize where

import Core.MakeIMG
import Core.Dither
 
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

type DrawMax = (Int,Int)
type ImgMax = (Int,Int)


-- goResize inF outF h = either error f =<< readImage inF
--     where f (ImageRGB8 i) = if fact < 1 
--                                 then do putStrLn $ "making " ++ outF
--                                         savePngImage outF . ImageRGB8 $ resize fact i   
--                                 else error "only shrinks"
--                             where fact = h % (imageHeight i)
--           f _             = error "only does ImageRGB8"         
 
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














