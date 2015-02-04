module Dither where 

import           Control.Applicative (Applicative (..), (<$>))
import           Data.Bits           (shiftR, unsafeShiftL, unsafeShiftR, (.&.), (.|.))
import           Data.List           (elemIndex)
import           Data.Maybe          (fromMaybe)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Word           

import           Data.Vector         (Vector, (!))
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

import           Codec.Picture.Types
import           Control.Monad.ST
-- Type Declarations
type Point = (Int,Int)
type Distance = Float
type DrawMax = Point 
type ImgMax = Point
type ConvFactor = Int
type Accessor = PixelRGB8 -> Pixel8

-- HelperVariables for Testing
redPix   = PixelRGB8 255 0   0
greenPix = PixelRGB8 0   255 0
bluePix  = PixelRGB8 0   0   255
blackPix = PixelRGB8 0   0   0
whitePix = PixelRGB8 255 255 255
yellowPix  = PixelRGB8 255 255 0
magentaPix = PixelRGB8 255 0 255
cyanPix    = PixelRGB8 0 255 255

listPix      = [redPix,greenPix,bluePix,blackPix,whitePix]
cmykPix      = [cyanPix,magentaPix,yellowPix,blackPix,whitePix]

-- Getters for pixel components, as the constructor does not
-- provide any public ones.
red, blue, green :: Accessor
red   (PixelRGB8 r _ _) = r
green (PixelRGB8 _ g _) = g
blue  (PixelRGB8 _ _ b) = b
-------------------------------------------------------------------------------
----            Dithering
-------------------------------------------------------------------------------

-- Add a dither mask to an image for ordered dithering.
-- Uses a small, spatially stable dithering algorithm based on magic numbers
-- and arithmetic inspired by the /a dither/ algorithm of Øyvind Kolås,
-- pippin@gimp.org, 2013. See, http://pippin.gimp.org/a_dither/.
dither :: Int -> Int -> PixelRGB8 -> PixelRGB8
dither x y (PixelRGB8 r g b) = PixelRGB8 (fromIntegral r')
                                         (fromIntegral g')
                                         (fromIntegral b')
  where
    -- Should view 16 as a parameter that can be optimized for best looking
    -- results
    r' = min 255 (fromIntegral r + (x' + y') .&. 16)
    g' = min 255 (fromIntegral g + (x' + y' + 7973) .&. 16)
    b' = min 255 (fromIntegral b + (x' + y' + 15946) .&. 16)
    x' = 119 * x
    y' = 28084 * y



-- ditherFloydRGB8 :: [PixelRGB8] -> Image PixelRGB8 -> Image PixelRGB8
-- ditherFloydRGB8 [] img = img
-- ditherFloydRGB8 ls img@(Image { imageWidth  = w, 
--                                 imageHeight = h, 
--                                 imageData   = vec }) =
--   Image w h vecSweep
--     where sourceComponentCount = componentCount (undefined :: PixelRGB8)
--           vecSweep = lineSweep vec 0 0
--           lineSweep arrY lineX lineY
--             | lineY >= h = arrY
--             | otherwise  = colSweep arrY lineX
--             where colSweep arrX colX
--                     | colX >= w  = lineSweep arrX 0 (lineY+1)
--                     | otherwise  = colSweep (dither arrX) (colX+1)
--                     where dither arrD = arrD VS.// (resVal resPix) ++ errorRes


--                           pixofPos = PixelRGB8  (arrX VS.! (baseId + 0))
--                                                 (arrX VS.! (baseId + 1))
--                                                 (arrX VS.! (baseId + 2))
--                           baseId   = (colX + lineY * w) * sourceComponentCount
--                           resPix   = colorMinDist pixofPos listPix
--                           resVal (PixelRGB8 r g b) = [((baseId + 0),r),((baseId + 1),g),((baseId + 2),b)]
--                           pixError = subtPixel pixofPos resPix
--                           posErrls = [(7,(colX+1,lineY)),(3,(colX-1,lineY)),(5,(colX,lineY+1)),(1,(colX+1,lineY+1))]
--                           toBase (col,lin) = (col + lin * w) * sourceComponentCount
--                           errorRes = map toVal posErrls []
--                           toVal []     acc = acc
--                           toVal (x:xs) acc = toVal xs $ (toErr x):acc
--                           toErr (PixError a b c) (fac,pos)  = addWordError fac (toBase pos) :
--                         -- pixErrls = map posToPix posErrls 
--                         -- pixAtPos (x',y') = PixelRGB8  (arrD ! (baseId' + 0))
--                         --                             (arrD ! (baseId' + 1))
--                         --                             (arrD ! (baseId' + 2))
--                         --     where baseId' = (x' + y' * w) * sourceComponentCount
--                         -- resPixls = [(pos,res) | pos <- [], vecVal <- []] 
-- -- -- Pos, keine vecPos, keine pixofPos

type BasID = Int
type ErrorFac = Int

-- Things I KNOW::
-- w
-- h
-- compCount
-- (x,y)
-- vec
ditherFloydRGB8 :: [PixelRGB8] -> Image PixelRGB8 -> Image PixelRGB8
ditherFloydRGB8 [] img = img
ditherFloydRGB8 ls img@(Image { imageWidth  = w, 
                                imageHeight = h, 
                                imageData   = vec }) =
  Image w h vecSweep
    where compCount = componentCount (undefined :: PixelRGB8)
          vecSweep = lineSweep vec 0 0
          lineSweep arrY lineX y
            | y >= h = arrY
            | otherwise  = colSweep arrY lineX
            where colSweep arrX x
                    | x >= w  = lineSweep arrX 0 (y+1)
                    | otherwise  = colSweep (dither arrX) (x+1)
                    where dither arrD = arrD VS.// updateVals (findPix ls) errBases []

                          baseId :: Point -> Maybe BasID
                          baseId (a,b)
                              | a <  0    = Nothing
                              | a >= w    = Nothing
                              | b >= h    = Nothing
                              | otherwise = Just $ (a + b * w) * compCount

                          baseA :: BasID
                          baseA = (x + y * w) * compCount

                          findPix :: [PixelRGB8] -> (PixelRGB8, PixError)
                          findPix pxls = (newPix,pixErr)
                            where newPix = colorMinDist oldPix pxls
                                  oldPix = unsafePixelAt arrX baseA
                                  pixErr = subtPixel oldPix newPix

                          errBases :: [(ErrorFac, Maybe BasID)]
                          errBases         = [(7,(baseId (x+1,y  )))
                                             ,(3,(baseId (x-1,y  )))
                                             ,(5,(baseId (x  ,y+1)))
                                             ,(1,(baseId (x+1,y+1)))]

                          updateVals :: (PixelRGB8,PixError) -> [(ErrorFac, Maybe BasID)] -> [(BasID,Word8)] -> [(BasID,Word8)]
                          updateVals   ((PixelRGB8 rn gn bn),_  ) []     acc = (baseA,rn):(baseA+1,gn):(baseA+2,bn):acc
                          updateVals v@(p                   ,err) (x:xs) acc = (shareError x err) ++ (updateVals v xs acc)
                            where
                            shareError (fac, Nothing   ) (PixError r' g' b')= []
                            shareError (fac,(Just base)) (PixError r' g' b')= addError oldPix
                              where
                              oldPix = unsafePixelAt arrX base
                              addError (PixelRGB8 r g b) = [(base,(calcErr r r')),(base+1,(calcErr g g')),(base+2,(calcErr b b'))]
                              calcErr p' e'
                                      | res <= 0   = 0
                                      | res >= 255 = 255
                                      | otherwise  = fromIntegral res
                                      where res = fromIntegral p' + ((fac*(fromIntegral e') `shiftR` 4))




data PixError = PixError Int Int Int 
    deriving (Show,Eq,Ord)

--((PixelRGB8,PixError),[(Int,Point,PixelRGB8)])

toVecPos :: Int -> (Point, PixelRGB8) -> [(Int,Word8)]
toVecPos w ((x,y),(PixelRGB8 r g b)) = [((baseInd + 0),r),((baseInd + 1),g),((baseInd + 2),b)]
  where baseInd = (x + y * w) * componentCount (undefined :: PixelRGB8)

addWordError :: Int -> Word8 -> Int -> Word8
addWordError fac val err
            | res <= 0   = 0
            | res >= 255 = 255
            | otherwise  = fromIntegral res
            where res = fromIntegral val + ((fac*(fromIntegral err) `shiftR` 4))

addPixError :: Int -> PixelRGB8 -> PixError -> PixelRGB8
addPixError fact (PixelRGB8 r g b) (PixError x y z) = PixelRGB8 (pixError r x) (pixError g y) (pixError b z)
  where pixError u v
            | res <= 0   = 0
            | res >= 255 = 255
            | otherwise  = fromIntegral res
            where res = fromIntegral u + ((fact*(fromIntegral v) `shiftR` 4))

errPix = PixelRGB8 100 100

--posToPix :: (Int,Point) -> (Int,Point,PixelRGB8)

subtPixel :: PixelRGB8 -> PixelRGB8 -> PixError
subtPixel (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = PixError r' g' b'
    where
    r' = fromIntegral r1 - fromIntegral r2
    g' = fromIntegral g1 - fromIntegral g2
    b' = fromIntegral b1 - fromIntegral b2

colorMinDist :: PixelRGB8 -> [PixelRGB8] -> PixelRGB8
colorMinDist p []     = PixelRGB8 255 255 255
colorMinDist p (x:xs) = colorMinDistAcc p xs ((colorDist8 p x),x) 
    where
    colorMinDistAcc :: PixelRGB8 -> [PixelRGB8] -> (Distance,PixelRGB8) -> PixelRGB8
    colorMinDistAcc p []     (_,pixAcc)    = pixAcc
    colorMinDistAcc p (x:xs) (dist,pixAcc)
        | dist' < dist = colorMinDistAcc p xs (dist',x)
        | otherwise    = colorMinDistAcc p xs (dist ,pixAcc)
        where
        dist' = colorDist8 p x 

colorDist8 :: PixelRGB8 -> PixelRGB8 -> Distance
colorDist8 (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = sqrt $ (2 + r'/256) * dr^2 + 4 * dg^2 + (2+(255-r'/256)*db^2)
    where
    r' = (fromIntegral r1)/2 + (fromIntegral r2)/2
    dr = fromIntegral r1 - fromIntegral r2
    dg = fromIntegral g1 - fromIntegral g2
    db = fromIntegral b1 - fromIntegral b2


getPixCoords :: ImgMax -> ConvFactor -> [Point]
getPixCoords (imx,imy) f = [(x,y) | x <- [0,f..imx-1], y <- [0,f..imy-1]]

-- arrPix :: Point ->[PixelRGB8]
-- arrPix p = pToPix $ imgToPoints p (varConF-1) varImgMax
--     where
--     imgToPoints :: Point -> ConvFactor -> ImgMax -> [Point]
--     imgToPoints (x,y) f (xmax,ymax) = [(x',y')| 
--                                             x' <- [x..(x+f)], 
--                                             y' <- [y..(y+f)], 
--                                             x' <= (xmax-1),
--                                             y' <= (ymax-1)]
--     pToPix :: [Point] -> [PixelRGB8]
--     pToPix []     = []
--     pToPix (p:ps) = (uncurry (pixelAt varImg8) p): pToPix ps

-- convF :: ImgMax -> DrawMax -> Int
-- convF (imgX,imgY) (drawX,drawY)
--     | imgX >= imgY   = truncate $ fromIntegral $ imgX `div` drawX
--     | otherwise      = truncate $ fromIntegral $ imgY `div` drawX

getvarImgMax :: Image PixelRGB8 -> ImgMax
getvarImgMax img = (imageWidth img,imageHeight img)    






-- pixelsMap :: (Point -> [PixelRGB8])-> ([PixelRGB8] -> PixelRGB8) -> Image PixelRGB8 -> Image PixelRGB8
-- pixelsMap f g Image { imageWidth = w, imageHeight = h, imageData = vec } =
--   Image w h pixels
--     where compCount = componentCount (undefined :: PixelRGB8)

--           pixels = runST $ do
--             newArr <- M.new (w * h * compCount)
--             let lineMapper _ _ y | y >= h = return ()
--                 lineMapper readIdxLine writeIdxLine y = colMapper readIdxLine writeIdxLine 0
--                   where colMapper readIdx writeIdx x
--                             | x >= w = lineMapper readIdx writeIdx $ y + 1
--                             | otherwise = do
--                                 pixList <- f (x,y)
--                                 unsafeWritePixel newArr writeIdx . f $ unsafePixelAt vec readIdx
--                                 colMapper (readIdx  + compCount)
--                                           (writeIdx + compCount)
--                                           (x + 1)
--             lineMapper 0 0 0

--             -- unsafeFreeze avoids making a second copy and it will be
--             -- safe because newArray can't be referenced as a mutable array
--             -- outside of this where block
--             V.unsafeFreeze newArr

-- arrPix 

-- fibST :: Integer -> Integer
-- fibST n = 
--     if n < 2
--     then n
--     else runST $ do
--         x <- newSTRef 0
--         y <- newSTRef 1
--         fibST' n x y
 
--     where fibST' 0 x _ = readSTRef x
--           fibST' n x y = do
--               x' <- readSTRef x
--               y' <- readSTRef y
--               writeSTRef x y'
--               writeSTRef y $! x'+y'
--               fibST' (n-1) x y











