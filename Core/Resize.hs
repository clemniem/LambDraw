module Core.Resize where


import Codec.Picture
import Control.Monad(join)

import Data.Ratio
import Control.Arrow
import Control.Applicative


-- DIN A4 210 x 297 mm
-- DIN A5 148 x 210 mm
-- DIN A6 105 x 148 mm
-- DIN A7 74 x 105 mm

-------------------------------------------------------------------------------
----            Resize Function
-------------------------------------------------------------------------------

-- | source: https://gist.github.com/eflister/5456125
--resize :: (RealFrac a, Pixel b) => a -> Image b -> Image b
resize :: (RealFrac a) => a -> Image PixelRGB8 -> Image PixelRGB8
resize fact i = uncurry (generateImage f) new
    where f = curry $ (pixelAt' old (round $ 1/fact) i) . (uncurry (***) $ (join (***) tmp) (fst,snd))
          old = (imageWidth &&& imageHeight) i
          new = (join (***) $ scale fact) old
          scale r = round . (* (toRational r)) . toRational
          tmp s = scale (s old) . (% (s new))
 
-- | eflister: "pretty slow, should use repa or something"
pixelAt' :: (Int, Int) -> Int -> Image PixelRGB8 -> (Int, Int) -> PixelRGB8
pixelAt' (dw,dh) s i (x,y) = avg pix
    where inds n d = [a | a <- (+ n) <$> [0..s], all id ([(>= 0), (< d)] <*> [a])]
          pix = (uncurry $ pixelAt i) <$> [(x',y') | x' <- inds x dw, y' <- inds y dh]
          avg p = (foldl pp (0,0,0) p) `pd` (length p)
          pp (r, g, b) (PixelRGB8 r' g' b') = (pf r r', pf g g', pf b b')
          pf a = (+ a) . fromIntegral
          pd (r, g, b) d = PixelRGB8 (pr r d) (pr g d) (pr b d)
          pr a b = round (a % b)













