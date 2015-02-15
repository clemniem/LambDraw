module Colorsplicer where

import Dither
import MakeIMG

import Codec.Picture.Types
import Data.List as L
import Data.Ord
import           Data.Vector          (Vector, (!))
import qualified Data.Vector.Storable as V
import           Control.Monad( foldM, liftM, ap )
import           Control.Monad.ST as ST
import           Control.Monad.Primitive ( PrimMonad, PrimState )
import           Data.Bits( unsafeShiftL, unsafeShiftR, (.|.), (.&.),shiftR )
import           Data.Word( Word8, Word16, Word32, Word64 )
import qualified Data.Vector.Storable.Mutable as M
import           Data.Maybe


dynDith = ImageRGB8 picDith

picDith = ditherFloydRGB8 rgbPixls pic
picPath = generateImage (pixelungls $ starSort 50 50 testList) 50 50

-- Checks Pixel == Pixel 
checkColor :: PixelRGB8 -> PixelRGB8 -> Bool
checkColor (PixelRGB8 a1 a2 a3) (PixelRGB8 b1 b2 b3)
    | a1 /= b1  = False
    | a2 /= b2  = False 
    | a3 /= b3  = False
    | otherwise = True

-- creates List of Points for one Color from Image PixelRGB8
colorSplicer :: Image PixelRGB8 -> PixelRGB8 -> [Point] 
colorSplicer img@(Image { imageWidth  = w, 
                          imageHeight = h }) pix =
    [(x,y)| x <- [0..w-1], y <-[0..h-1], checkColor (pixelAt img x y) pix]


saveDith = saveImage "./images/picDith" $ ImageRGB8 $ picDith
savePicPath = saveImage "./images/picPath" $ ImageRGB8 $ picPath

testList = colorSplicer picDith bluePix
dshortestPath = distSort $ (0,0):testList
mathShortestPath = distOptimizer $ (0,0):testList
shortestPath = L.reverse $ snd dshortestPath

starls = starSort 50 50 testList
lengthStarls = lsDistance starls

smallList = [(x,y) | x <- [0,2..40], y<-[0..20]] :: [Point]

lsDistance :: [Point] -> Distance
lsDistance []     = 0
lsDistance (a:[]) = 0
lsDistance (a:b:cs) = distance a b + lsDistance (b:cs)

-- sortByNext :: [Point] -> [Point]
-- sortByNext [] = []
-- sortByNext (x:xs) = x : getNext x xs 
--     getNext p pts = 



-- STARSORT *
--
--  \ 1|2 /
--  8\ | /3
-- ----c----
--  7/ | \4
--  /6 |5 \
starSort :: Int -> Int -> [Point] -> [Point]
starSort w h []  = []
starSort w h pts = (  
                       ds [(x,y) | (x,y) <- nw, x>y   ]
                    ++ ds (L.reverse $ sortByY [(x,y) | (x,y) <- no, x<=h-y]) -- Maybe needs sortByY
                    ++ ds (L.reverse [(x,y) | (x,y) <- no, x>h-y ]) -- Maybe needs L.reverse
                    ++ ds [(x,y) | (x,y) <- so, x>y   ]
                    ++ ds (L.reverse [(x,y) | (x,y) <- so, x<=y  ]) -- Maybe needs L.reverse
                    ++ ds (L.reverse [(x,y) | (x,y) <- sw, x>h-y ]) -- Maybe needs L.reverse
                    ++ ds (L.reverse $ sortByY [(x,y) | (x,y) <- sw, x<=h-y]) -- Maybe needs sortByY
                    ++ ds (L.reverse [(x,y) | (x,y) <- nw, x<=y  ])
                    ) 
    where nw = [(x,y) | (x,y) <- pts, x<=w', y<=h']
          no = [(x,y) | (x,y) <- pts, x>w' , y<=h']
          sw = [(x,y) | (x,y) <- pts, x<=w', y>h']
          so = [(x,y) | (x,y) <- pts, x>w' , y>h']
          w' = truncate $ fromIntegral w/2
          h' = truncate $ fromIntegral h/2
          ds ls = L.reverse $ snd $ distSort ls 

-- calculates euclidian distance between two Points.
distance:: Point -> Point -> Distance
distance (x1,y1) (x2,y2) = sqrt $ fromIntegral $ (x1-x2)^2 + (y1-y2)^2

-- calculates the closest Point from a list to a given Pint. 
-- Returns: (closest Point, distance, rest of the list)
nextPoint :: Point -> [Point] -> (Maybe Point,Distance,[Point])
nextPoint p [] = (Nothing,0,[])
nextPoint p origls@(x:xs)
    | elem p origls = nextPoint p $ L.delete p origls
    | otherwise = nextPointAcc (distance p x) (p,x) xs
    where
    nextPointAcc :: Distance -> (Point,Point) -> [Point] -> (Maybe Point,Distance,[Point])
    nextPointAcc acc (p,np) [] = (Just np, acc ,delete np origls) 
    nextPointAcc acc (p,np) (x:xs)
        | distance p x < acc = nextPointAcc (distance p x) (p,x) xs
        | otherwise = nextPointAcc acc (p,np) xs

-- sorts a List of Points by their Y Coordinate
sortByY :: [Point] -> [Point]
sortByY = sortBy (comparing snd)

-- sorts a List of Points by their X Coordinate
sortByX :: [Point] -> [Point]
sortByX = sort

-- Takes a List of Points, starts with the head and sorts by the closest Point
distSort :: [Point] -> (Maybe Distance,[Point])
distSort [] = (Nothing,[])
distSort (x:xs) = distSortAcc (nextPoint x xs) (Just 0,[x])
    where
    distSortAcc :: (Maybe Point,Distance,[Point]) -> (Maybe Distance,[Point]) -> (Maybe Distance,[Point]) 
    distSortAcc (Nothing,_,[]) acc = acc
    distSortAcc (Just p,dist,xs) (distAcc,lsAcc) = distSortAcc (nextPoint p xs) (fmap (+dist) distAcc,p:lsAcc)

-- Calculates the sorted List of closest Points for every Point in the List 
-- returns the one with minimal Distance
distOptimizer :: [Point] -> (Maybe Float,[Point])
distOptimizer [] = (Nothing, [])
distOptimizer ls@(x:xs) = distOptimizerAcc ls xs $ distSort ls
    where
    distOptimizerAcc :: [Point] -> [Point] -> (Maybe Float,[Point]) -> (Maybe Float,[Point])
    distOptimizerAcc origls [] acc = acc
    distOptimizerAcc origls countls@(x:xs) acc
        | fst (distSort (x:origls)) < fst acc = distOptimizerAcc origls xs (distSort (x:origls))
        | otherwise = distOptimizerAcc origls xs acc



generateImagels :: 
              [Point]       -- ^ List of Points where a Pixel should be
              -> Int        -- ^ Width in pixels
              -> Int        -- ^ Height in pixels
              -> Image PixelRGB8
generateImagels ls w h = Image { imageWidth = w, imageHeight = h, imageData = generated }
  where compCount = componentCount (undefined :: PixelRGB8)
        generated = runST $ do
            arr <- M.new (w * h * compCount)
            let lineGenerator _ y | y >= h = return ()
                lineGenerator lineIdx y = column lineIdx 0
                  where column idx x | x >= w = lineGenerator idx $ y + 1
                        column idx x = do
                            unsafeWritePixel arr idx (PixelRGB8 255 255 255)
                            column (idx + compCount) $ x + 1

            lineGenerator 0 0
            blub w arr 0 ls
            V.unsafeFreeze arr

--blub :: [Point
blub w arr _     []     = return ()
blub w arr count ((x,y):ps) = do unsafeWritePixel arr p' (PixelRGB8 count 255 count')
                                 blub w arr (count+1) ps
                                  where p' = (x+y*w)*3
                                        count' = if count < 10
                                                    then 255
                                                    else 0






























