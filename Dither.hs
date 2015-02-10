module Dither where 

import           Data.Vector          (Vector, (!))
import qualified Data.Vector.Storable as VS
import           Codec.Picture.Types
import Control.Monad( foldM, liftM, ap )
import Control.DeepSeq( NFData( .. ) )
import Control.Monad.ST (runST)
import Control.Monad.Primitive ( PrimMonad, PrimState )
import Foreign.ForeignPtr( castForeignPtr )
import Foreign.Storable ( Storable )
import Data.Bits( unsafeShiftL, unsafeShiftR, (.|.), (.&.),shiftR )
import Data.Word( Word8, Word16, Word32, Word64 )
import Data.List( foldl' )
import Data.Vector.Storable ( (!) )
import qualified Data.Vector.Storable.Mutable as M

-- Type & Data declarations
type Point      = (Int,Int)
type Distance   = Float
data PixError   = PixError Int Int Int 
    deriving (Show,Eq,Ord)
type BasID      = Int
type ErrorFac   = Int

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

-- operation to drop used colors to a list of colors
noDither :: [PixelRGB8] -> PixelRGB8 -> PixelRGB8
noDither ls p = colorMinDist p ls

-------------------------------------------------------------------------------
----            Dithering
-------------------------------------------------------------------------------
-- Floyd-Steinberg Algorithm for a Palette (= [PixelRGB8]) and an Image PixelRGB8
ditherFloydRGB8 :: [PixelRGB8] -> Image PixelRGB8 -> Image PixelRGB8
ditherFloydRGB8 [] img = img
ditherFloydRGB8 ls img@(Image { imageWidth  = w, 
                                imageHeight = h, 
                                imageData   = vec }) =
  Image w h vecSweep
    where 
    compCount = componentCount (undefined :: PixelRGB8)
    vecSweep = lineSweep vec 0 0
    lineSweep arrY lineX y -- sweep lines 0 to h
      | y >= h = arrY
      | otherwise  = colSweep arrY lineX
      where 
      colSweep arrX x -- sweep columns 0 to w
        | x >= w  = lineSweep arrX 0 (y+1)
        | otherwise  = colSweep (dither arrX) (x+1)
        where 
        dither arrD = arrD VS.// updateVals (findPix ls) errBases []
        -- calculate indices where a pixel starts (= base) and check for bounds
        baseId :: Point -> Maybe BasID
        baseId (a,b)
            | a <  0    = Nothing
            | a >= w    = Nothing
            | b >= h    = Nothing
            | otherwise = Just $ (a + b * w) * compCount
        -- base of pixel of the current point (x,y)
        baseA :: BasID
        baseA = (x + y * w) * compCount
        -- calculate closest color from palette and pixError for current pixel
        findPix :: [PixelRGB8] -> (PixelRGB8, PixError)
        findPix pxls = (newPix,pixErr)
          where 
          newPix = colorMinDist oldPix pxls
          oldPix = unsafePixelAt arrX baseA
          pixErr = subtPixel oldPix newPix
        -- calculate error distribution bases
        errBases :: [(ErrorFac, Maybe BasID)]
        errBases         = [(7,(baseId (x+1,y  )))
                           ,(3,(baseId (x-1,y+1)))
                           ,(5,(baseId (x  ,y+1)))
                           ,(1,(baseId (x+1,y+1)))]
        -- calculate list of updates for //-operator
        updateVals :: (PixelRGB8,PixError) -> [(ErrorFac, Maybe BasID)] -> [(BasID,Word8)] -> [(BasID,Word8)]
        updateVals   ((PixelRGB8 rn gn bn),_  ) []     acc = (baseA,rn):(baseA+1,gn):(baseA+2,bn):acc
        updateVals v@(p                   ,err) (x:xs) acc = (shareError x err) ++ (updateVals v xs acc)
          where
          shareError (fac, Nothing   ) (PixError r' g' b')= []
          shareError (fac,(Just base)) (PixError r' g' b')= addError oldPix'
            where
            oldPix' = unsafePixelAt arrX base
            addError (PixelRGB8 r g b) = [(base,(calcErr r r')),(base+1,(calcErr g g')),(base+2,(calcErr b b'))]
            calcErr :: Word8 -> Int -> Word8
            calcErr p' e'
                    | res <= 0   = 0
                    | res >= 255 = 255
                    | otherwise  = fromIntegral res
                    where res = fromIntegral p' + ((fac*(fromIntegral e') `shiftR` 4))

calcErr' :: Word8 -> Int -> Int -> Word8
calcErr' p' e' fac
        | res <= 0   = 0
        | res >= 255 = 255
        | otherwise  = fromIntegral res
        where res = fromIntegral p' + ((fac*(fromIntegral e') `shiftR` 4))


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



ditherST :: [PixelRGB8] -> Image PixelRGB8 -> Image PixelRGB8
ditherST []   img                               = img 
ditherST pxls img@(Image { imageWidth  = w, 
                           imageHeight = h, 
                           imageData   = vec }) =
  Image w h pixels
    where sourceCompCount    = componentCount (undefined :: PixelRGB8)
          destComponentCount = componentCount (undefined :: PixelRGB8)

          pixels = runST $ do
            newArr <- M.new (w * h * sourceCompCount)
            let lineMapper _ _ y | y >= h = return ()
                lineMapper readIdxLine writeIdxLine y = colMapper readIdxLine writeIdxLine 0
                  where colMapper readIdx writeIdx x
                            | x >= w = lineMapper readIdx writeIdx $ y + 1
                            | otherwise = do
                                unsafeWritePixel newArr writeIdx $ fst findPix
                                unsafeWritePixel' 7 (baseId (x+1,y  )) $ snd findPix
                                colMapper (readIdx  + sourceCompCount)
                                          (writeIdx + destComponentCount)
                                          (x + 1)
                                    where
                                    -- calculate closest color from palette and pixError for current pixel                                    
                                    findPix :: (PixelRGB8, PixError)
                                    findPix = (newPix,pixErr)
                                      where 
                                      newPix = colorMinDist oldPix pxls
                                      oldPix = unsafePixelAt vec readIdx
                                      pixErr = subtPixel oldPix newPix
                                    -- calculate indices where a pixel starts (= base) and check for bounds
                                    baseId :: Point -> Maybe Int
                                    baseId (a,b)
                                        | a <  0    = Nothing
                                        | a >= w    = Nothing
                                        | b >= h    = Nothing
                                        | otherwise = Just $ (a + b * w) * sourceCompCount
                                    unsafeWritePixel' fac Nothing    pe  = return ()
                                    unsafeWritePixel' fac (Just bas) pe  = unsafeWritePixel newArr bas $ findErrPix fac bas pe
                                    findErrPix :: Int -> Int -> PixError -> PixelRGB8
                                    findErrPix fac' bas' pe' = newPix
                                      where 
                                      newPix = compwiseErr pe' oldPix
                                      oldPix = unsafePixelAt vec bas'
                                      compwiseErr (PixError r' g' b') (PixelRGB8 r g b) = PixelRGB8 (calcErr r r') (calcErr g g') (calcErr b b')
                                      calcErr :: Pixel8 -> Int -> Pixel8
                                      calcErr p' e'
                                                | res <= 0   = 0
                                                | res >= 255 = 255
                                                | otherwise  = fromIntegral res
                                                where res = fromIntegral p' + ((fac'*(fromIntegral e') `shiftR` 4))


            lineMapper 0 0 0

            -- unsafeFreeze avoids making a second copy and it will be
            -- safe because newArray can't be referenced as a mutable array
            -- outside of this where block
            VS.unsafeFreeze newArr

                                    -- -- calculate indices where a pixel starts (= base) and check for bounds
                                    -- baseId :: Point -> Maybe Int
                                    -- baseId (a,b)
                                    --     | a <  0    = Nothing
                                    --     | a >= w    = Nothing
                                    --     | b >= h    = Nothing
                                    --     | otherwise = Just $ (a + b * w) * sourceCompCount
                                    -- baseErF :: Int -> Maybe Int
                                    -- baseErF 7 = baseId (x+1,y  ) 
                                    -- baseErF 3 = baseId (x-1,y+1)
                                    -- baseErF 5 = baseId (x  ,y+1)
                                    -- baseErF 1 = baseId (x+1,y+1)
                                    -- findErrPix :: Int -> Int -> PixError -> PixelRGB8
                                    -- findErrPix fac bas pe = newPix
                                    --   where 
                                    --   newPix = compwiseErr fac pe oldPix
                                    --   oldPix = unsafePixelAt vec bas
                                    --   compwiseErr fac' (PixError r' g' b') (PixelRGB8 r g b) = PixelRGB8 (calcErr r r') (calcErr g g') (calcErr b b')
                                    --   calcErr :: Pixel8 -> Int -> Pixel8
                                    --   calcErr p' e'
                                    --             | res <= 0   = 0
                                    --             | res >= 255 = 255
                                    --             | otherwise  = fromIntegral res
                                    --             where res = fromIntegral p' + ((fac*(fromIntegral e') `shiftR` 4))






-- -- Dither with ST Monad
-- -- Resizing Image by calculating average pixel Values
-- ditherST :: Image PixelRGB8 -> Image PixelRGB8
-- ditherST img@(Image { imageWidth  = w, 
--                          imageHeight = h, 
--                          imageData   = vec }) =
--   Image w h pixels
--     where compCount = componentCount (undefined :: PixelRGB8)
--           pixels = runST $ do
--             newArr <- M.new (w * h * compCount)
--             let lineMapper _ _ y | y >= h = return ()
--                 lineMapper readIdxLine writeIdxLine y = colMapper readIdxLine writeIdxLine 0
--                   where colMapper readIdx writeIdx x
--                             | x >= w = lineMapper readIdx writeIdx $ y + 1
--                             | otherwise = do
--                                 pixErr <- findPix
--                                 unsafeWritePixel newArr writeIdx $ fst pixErr
--                                 writeError 7 (baseErF 7) (snd pixErr)
--                                 writeError 3 (baseErF 3) (snd pixErr)                                
--                                 writeError 5 (baseErF 5) (snd pixErr)
--                                 writeError 1 (baseErF 1) (snd pixErr)
--                                 colMapper (readIdx  + compCount)
--                                           (writeIdx + compCount)
--                                           (x + 1)
--                                     where
--                                     -- calculate indices where a pixel starts (= base) and check for bounds
--                                     baseId :: Point -> Maybe Int
--                                     baseId (a,b)
--                                         | a <  0    = Nothing
--                                         | a >= w    = Nothing
--                                         | b >= h    = Nothing
--                                         | otherwise = Just $ (a + b * w) * compCount
--                                     -- base of pixel of this instant point (x,y)
--                                     -- baseA :: BasID
--                                     -- baseA = (x + y * w) * compCount
--                                     -- calculate closest color from palette and pixError for current pixel                                    
--                                     findPix :: [PixelRGB8] -> (PixelRGB8, PixError)
--                                     findPix pxls = (newPix,pixErr)
--                                       where 
--                                       newPix = colorMinDist oldPix rgbPixls
--                                       oldPix = unsafePixelAt vec readIdx
--                                       pixErr = subtPixel oldPix newPix
--                                     writeError _   Nothing    _  = return ()
--                                     writeError fac (Just bas) pe = unsafeWritePixel newArr bas $ findErrPix fac bas pe 
--                                     baseErF :: Int -> Maybe Int
--                                     baseErF 7 = baseId (x+1,y  ) 
--                                     baseErF 3 = baseId (x-1,y+1)
--                                     baseErF 5 = baseId (x  ,y+1)
--                                     baseErF 1 = baseId (x+1,y+1)
--                                     unsafeWritePixel' (fac,Just bId) pe' = unsafeWritePixel newArr bId $ findErrPix fac bId pe'
--                                     findErrPix :: Int -> Int -> PixError -> PixelRGB8
--                                     findErrPix fac bas pe = newPix
--                                       where 
--                                       newPix = compwiseErr fac pe oldPix
--                                       oldPix = unsafePixelAt vec bas
--                                       compwiseErr fac' (PixError r' g' b') (PixelRGB8 r g b) = PixelRGB8 (calcErr r r') (calcErr g g') (calcErr b b')
--                                       calcErr :: Word8 -> Int -> Word8
--                                       calcErr p' e'
--                                                 | res <= 0   = 0
--                                                 | res >= 255 = 255
--                                                 | otherwise  = fromIntegral res
--                                                 where res = fromIntegral p' + ((fac*(fromIntegral e') `shiftR` 4))
--                                     -- calculate error distribution bases
--                                     -- errBases :: [(ErrorFac, Maybe BasID)]
--                                     -- errBases         = [(7,(baseId (x+1,y  )))
--                                     --                    ,(3,(baseId (x-1,y+1)))
--                                     --                    ,(5,(baseId (x  ,y+1)))
--                                     --                    ,(1,(baseId (x+1,y+1)))]
--                                     -- calculate list of updates for //-operator
--                                     -- updateVals :: (PixelRGB8,PixError) -> [(ErrorFac, Maybe BasID)] -> [(BasID,Word8)] -> [(BasID,Word8)]
--                                     -- updateVals   ((PixelRGB8 rn gn bn),_  ) []     acc = (baseA,rn):(baseA+1,gn):(baseA+2,bn):acc
--                                     -- updateVals v@(p                   ,err) (x:xs) acc = (shareError x err) ++ (updateVals v xs acc)
--                                     --   where
--                                     --   shareError (fac, Nothing   ) (PixError r' g' b')= []
--                                     --   shareError (fac,(Just base)) (PixError r' g' b')= addError oldPix'
--                                     --     where
--                                     --     oldPix' = unsafePixelAt vec base
--                                     --     addError (PixelRGB8 r g b) = [(base,(calcErr r r')),(base+1,(calcErr g g')),(base+2,(calcErr b b'))]
--                                     --     calcErr :: Word8 -> Int -> Word8
--                                     --     calcErr p' e'
--                                     --             | res <= 0   = 0
--                                     --             | res >= 255 = 255
--                                     --             | otherwise  = fromIntegral res
--                                     --             where res = fromIntegral p' + ((fac*(fromIntegral e') `shiftR` 4))
--             lineMapper 0 0 0

--             -- unsafeFreeze avoids making a second copy and it will be
--             -- safe because newArray can't be referenced as a mutable array
--             -- outside of this where block
--             VS.unsafeFreeze newArr