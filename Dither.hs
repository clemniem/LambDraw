{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}

module Dither where 

import MakeIMG
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


-- operation to drop used colors to a list of colors
noDither :: [PixelRGB8] -> PixelRGB8 -> PixelRGB8
noDither ls p = colorMinDist p ls

-------------------------------------------------------------------------------
----            Dithering
-------------------------------------------------------------------------------

-- calculates the difference between two pixels
-- saves values as Int so negative values can be saved 
subtPixel :: PixelRGB8 -> PixelRGB8 -> PixError
subtPixel (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = PixError r' g' b'
    where
    r' = fromIntegral r1 - fromIntegral r2
    g' = fromIntegral g1 - fromIntegral g2
    b' = fromIntegral b1 - fromIntegral b2

-- takes a pixel and a palette (= list of pixels)
-- returns the perceived closest pixel from the palette
colorMinDist :: PixelRGB8 -> [PixelRGB8] -> PixelRGB8
colorMinDist p []     = PixelRGB8 255 255 255
colorMinDist p (x:xs) = colorMinDistAcc p xs ((colorDist8 p x),x) 
    where
    colorMinDistAcc :: PixelRGB8 -> [PixelRGB8] -> (Distance,PixelRGB8) -> PixelRGB8
    colorMinDistAcc p []     (_   ,pixAcc)    = pixAcc
    colorMinDistAcc p (x:xs) (dist,pixAcc)
        | dist' < dist = colorMinDistAcc p xs (dist',x)
        | otherwise    = colorMinDistAcc p xs (dist ,pixAcc)
        where
        dist' = colorDist8 p x 

colorDistGrey :: PixelRGB8 -> PixelRGB8
colorDistGrey (PixelRGB8 g1 g2 g3)
                      | (and [g1==g2,g1==g3,g1>= 123]) = whitePix
                      | otherwise = blackPix

-- calculates perceived color-distance between two pixels
-- Source for function: http://www.compuphase.com/cmetric.htm
colorDist8 :: PixelRGB8 -> PixelRGB8 -> Distance
colorDist8 (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = sqrt $ (2 + r'/256)   * dr^2 
                                                            +  4             * dg^2 
                                                            + (2+(255-r'/256)* db^2)
    where
    r' = (fromIntegral r1)/2 + (fromIntegral r2)/2
    dr = fromIntegral r1 - fromIntegral r2
    dg = fromIntegral g1 - fromIntegral g2
    db = fromIntegral b1 - fromIntegral b2

-- simple euclidian Distance between two pixels
colorDistEuclid ::  PixelRGB8 -> PixelRGB8 -> Distance
colorDistEuclid (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = sqrt $ dr^2  + dg^2 + db^2
    where
    r' = (fromIntegral r1)/2 + (fromIntegral r2)/2
    dr = fromIntegral r1 - fromIntegral r2
    dg = fromIntegral g1 - fromIntegral g2
    db = fromIntegral b1 - fromIntegral b2

-- used to calculate Neighbour-Points
addTupel :: Num a => (a,a) -> (a,a) -> (a,a)
addTupel (x1,x2) (y1,y2) = (x1 + y1, x2 + y2)

-- Floyd-Steinberg Algorithm for a Palette (= [PixelRGB8]) and an Image PixelRGB8
ditherFloydRGB8 :: [PixelRGB8] -> Image PixelRGB8 -> Image PixelRGB8
ditherFloydRGB8 []   img                               = img 
ditherFloydRGB8 pxls img@(Image { imageWidth  = w, 
                           imageHeight = h, 
                           imageData   = vec }) =
  Image w h pixels
    where sourceCompCount    = componentCount (undefined :: PixelRGB8)
          destComponentCount = componentCount (undefined :: PixelRGB8)

          pixels = runST $ do
            oldArr <- VS.thaw vec 
            --newArr <- M.new (w * h * sourceCompCount)
            let lineMapper _ _ y | y >= h = return ()
                lineMapper readIdxLine writeIdxLine y = colMapper readIdxLine writeIdxLine 0 
                  where colMapper readIdx writeIdx x
                            | x >= w    = lineMapper readIdx writeIdx $ y + 1
                            | otherwise = do
                                oldPix <- unsafeReadPixel' oldArr readIdx
                                newPix <- return $ colorMinDist oldPix pxls 
                                unsafeWritePixel oldArr writeIdx newPix
                                errPix <- return $ subtPixel oldPix newPix                              
                                unsafeWritePixel' oldArr [(7,(1,0)),(3,((-1),1)),(5,(0,1)),(1,(1,1))] errPix
                                colMapper (readIdx  + sourceCompCount)
                                          (writeIdx + destComponentCount)
                                          (x + 1)
                                    where
                                    -- Still needed as it only works with RGB8 Pixels uptil now
                                    unsafeReadPixel' :: PrimMonad m => M.STVector (PrimState m) (PixelBaseComponent PixelRGB8) -> Int -> m PixelRGB8
                                    unsafeReadPixel' vec idx =
                                            PixelRGB8 `liftM` M.unsafeRead vec idx
                                                      `ap`    M.unsafeRead vec (idx + 1)
                                                      `ap`    M.unsafeRead vec (idx + 2)
                                    -- Takes a List of Error Factors and Neighbours (Int,(Int,Int)) and shares the Error
                                    unsafeWritePixel' v []       _  = return ()
                                    unsafeWritePixel' v (eb:ebs) pe =  do if mbaseE == Nothing 
                                                                            then do unsafeWritePixel' v ebs pe
                                                                            else do oldPixE <- unsafeReadPixel' oldArr baseE 
                                                                                    unsafeWritePixel v baseE $ newPixE (fst eb) oldPixE
                                                                                    unsafeWritePixel' v ebs pe
                                                                          where
                                                                          newPixE fac ol = compwiseErr fac pe ol
                                                                          baseE          = fromJust mbaseE
                                                                          mbaseE         = baseId $ addTupel (x,y) $ snd eb 
                                    -- calculates the BaseId for any given Point in the Image (checks bounds)
                                    baseId :: Point -> Maybe Int
                                    baseId (a,b)
                                        | a <  0    = Nothing
                                        | a >= w    = Nothing
                                        | b >= h    = Nothing
                                        | otherwise = Just $ (a + b * w) * sourceCompCount
                                    -- adds the Error to a given Pixel with the corresponding Factor
                                    compwiseErr :: Int -> PixError -> PixelRGB8 -> PixelRGB8
                                    compwiseErr fac (PixError r' g' b') (PixelRGB8 r g b) = PixelRGB8 (calcErr r r') (calcErr g g') (calcErr b b')
                                      where
                                      calcErr :: Pixel8 -> Int -> Pixel8
                                      calcErr p' e'
                                                | res <= 0   = 0
                                                | res >= 255 = 255
                                                | otherwise  = fromIntegral res
                                                where res = fromIntegral p' + ((fac*(fromIntegral e') `shiftR` 4))


            lineMapper 0 0 0

            -- unsafeFreeze avoids making a second copy and it will be
            -- safe because newArray can't be referenced as a mutable array
            -- outside of this where block
            VS.unsafeFreeze oldArr

                                    
-- -- Floyd-Steinberg Algorithm for a Palette (= [PixelRGB8]) and an Image PixelRGB8
-- ditherFloydRGB8old :: [PixelRGB8] -> Image PixelRGB8 -> Image PixelRGB8
-- ditherFloydRGB8old [] img = img
-- ditherFloydRGB8old ls img@(Image { imageWidth  = w, 
--                                 imageHeight = h, 
--                                 imageData   = vec }) =
--   Image w h vecSweep
--     where 
--     compCount = componentCount (undefined :: PixelRGB8)
--     vecSweep = lineSweep vec 0 0
--     lineSweep arrY lineX y -- sweep lines 0 to h
--       | y >= h = arrY
--       | otherwise  = colSweep arrY lineX
--       where 
--       colSweep arrX x -- sweep columns 0 to w
--         | x >= w  = lineSweep arrX 0 (y+1)
--         | otherwise  = colSweep (dither arrX) (x+1)
--         where 
--         dither arrD = arrD VS.// updateVals (findPix ls) errBases []
--         -- calculate indices where a pixel starts (= base) and check for bounds
--         baseId :: Point -> Maybe BasID
--         baseId (a,b)
--             | a <  0    = Nothing
--             | a >= w    = Nothing
--             | b >= h    = Nothing
--             | otherwise = Just $ (a + b * w) * compCount
--         -- base of pixel of the current point (x,y)
--         baseA :: BasID
--         baseA = (x + y * w) * compCount
--         -- calculate closest color from palette and pixError for current pixel
--         findPix :: [PixelRGB8] -> (PixelRGB8, PixError)
--         findPix pxls = (newPix,pixErr)
--           where 
--           newPix = colorMinDist oldPix pxls
--           oldPix = unsafePixelAt arrX baseA
--           pixErr = subtPixel oldPix newPix
--         -- calculate error distribution bases
--         errBases :: [(ErrorFac, Maybe BasID)]
--         errBases         = [(7,(baseId (x+1,y  )))
--                            ,(3,(baseId (x-1,y+1)))
--                            ,(5,(baseId (x  ,y+1)))
--                            ,(1,(baseId (x+1,y+1)))]
--         -- calculate list of updates for //-operator
--         updateVals :: (PixelRGB8,PixError) -> [(ErrorFac, Maybe BasID)] -> [(BasID,Word8)] -> [(BasID,Word8)]
--         updateVals   ((PixelRGB8 rn gn bn),_  ) []     acc = (baseA,rn):(baseA+1,gn):(baseA+2,bn):acc
--         updateVals v@(p                   ,err) (x:xs) acc = (shareError x err) ++ (updateVals v xs acc)
--           where
--           shareError (fac, Nothing   ) (PixError r' g' b')= []
--           shareError (fac,(Just base)) (PixError r' g' b')= addError oldPix'
--             where
--             oldPix' = unsafePixelAt arrX base
--             addError (PixelRGB8 r g b) = [(base,(calcErr r r')),(base+1,(calcErr g g')),(base+2,(calcErr b b'))]
--             calcErr :: Word8 -> Int -> Word8
--             calcErr p' e'
--                     | res <= 0   = 0
--                     | res >= 255 = 255
--                     | otherwise  = fromIntegral res
--                     where res = fromIntegral p' + ((fac*(fromIntegral e') `shiftR` 4))









