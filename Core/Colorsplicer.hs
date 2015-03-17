module Core.Colorsplicer where

import Core.Dither
import Core.MakeIMG

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
import Control.Parallel.Strategies

-- helpers for debugging--------------------
dynDith = ImageRGB8 picDith

picDith = ditherFloydRGB8 rgbPixls pic
picPath = generateImage (pixelungls $ starSort picW picW testList) picW picW
saveDith = saveImage "Dith saved" "./images/picDith" $ ImageRGB8 $ picDith
savePicPath = saveImage "PicPath saved" "./images/picPath" $ ImageRGB8 $ picPath

testList = colorSplicer picDith bluePix
dshortestPath = distSort $ (0,0):testList
mathShortestPath = distOptimizer $ (0,0):testList
shortestPath = L.reverse $ snd dshortestPath

starlsPar  = starSortPar  picW picW testList
starls     = starSort     picW picW testList
starlsNODS = starSortNODS picW picW testList

lengthStarls = lsDistance starls

smallList = [(x,y) | x <- [0,2..40], y<-[0..20]] :: [Point]
-- helpers for debugging-------------END-------

-------------------------------------------------------------------------------
----            Splicing
-------------------------------------------------------------------------------

-- Checks Pixel == Pixel 
checkColor :: PixelRGB8 -> PixelRGB8 -> Bool
checkColor (PixelRGB8 a1 a2 a3) (PixelRGB8 b1 b2 b3)
    | a1 /= b1  = False
    | a2 /= b2  = False 
    | a3 /= b3  = False
    | otherwise = True

-- creates List of Points for one Color from Image PixelRGB8
colorSplicer :: Image PixelRGB8 -> PixelRGB8 -> [Point] 
colorSplicer img (PixelRGB8 255 255 255) = []
colorSplicer img@(Image { imageWidth  = w, imageHeight = h }) pix =
    [(x,y)| x <- [0..w-1], y <-[0..h-1], checkColor (pixelAt img x y) pix]


-------------------------------------------------------------------------------
----            Toolpath Sorting
-------------------------------------------------------------------------------
-- calculates euclidian distance between two Points.
distance:: Point -> Point -> Distance
distance (x1,y1) (x2,y2) = sqrt $ fromIntegral $ (x1-x2)^2 + (y1-y2)^2

--calculates the overall Distance of a [Point] starting at the head
lsDistance :: [Point] -> Distance
lsDistance []       = 0
lsDistance (a:[])   = 0
lsDistance (a:b:cs) = distance a b + lsDistance (b:cs)





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

-- distSortPre :: [Point] -> [Point]
-- distSortPre []         = []
-- distSortPre (p:[])     = [p]
-- distSortPre (pa:pb:ps) = pa : distSortPreAcc (distance pa pb) (pa,pb) ps (pb:ps)
--   where 
--   distSortPreAcc :: Distance -> (Point,Point) -> [Point] -> [Point] -> [Point]
--   distSortPreAcc d tup@(pa,pb) _ [] = pb : []
--   distSortPreAcc d tup@(pa,pb) [] rest@(r:rs)   = pb : distSortPreAcc (distance pb r) (pb,r) rest' rest'
--     where rest' = (delete pb rest)
--   distSortPreAcc d tup@(pa,pb) check@(pc:ps) rest@(r:rs) 
--       | d < xDiff pa pc = pb : distSortPreAcc (distance pb r) (pb,r) rest' rest'
--       | d < (distance pa pc) = distSortPreAcc d tup ps rest
--       | otherwise = distSortPreAcc d (pa,pc) ps rest 
--                   where xDiff (x1,y1) (x2,y2) = fromIntegral $ min (abs x2-x1) (abs y2-y1)
--                         rest' = (delete pb rest)


-- | STARSORT *
--
--  \ 1|2 /
--  8\ | /3
-- ----c----
--  7/ | \4
--  /6 |5 \
-- Divide Image into 8 Slices and Sort them with List Comprehension
starSort :: Int -> Int -> [Point] -> [Point]
starSort w h []  = []
starSort w h pts = (  
                       ds                      [(x,y) | (x,y) <- nw, x>y   ]
                    ++ ds (L.reverse $ sortByY [(x,y) | (x,y) <- no, x<=h-y]) -- Maybe needs sortByY
                    ++ ds (L.reverse           [(x,y) | (x,y) <- no, x>h-y ]) -- Maybe needs L.reverse
                    ++ ds                      [(x,y) | (x,y) <- so, x>y   ]
                    ++ ds (L.reverse           [(x,y) | (x,y) <- so, x<=y  ]) -- Maybe needs L.reverse
                    ++ ds (L.reverse           [(x,y) | (x,y) <- sw, x>h-y ]) -- Maybe needs L.reverse
                    ++ ds (L.reverse $ sortByY [(x,y) | (x,y) <- sw, x<=h-y]) -- Maybe needs sortByY
                    ++ ds (L.reverse           [(x,y) | (x,y) <- nw, x<=y  ])
                    ) 
    where nw = [(x,y) | (x,y) <- pts, x<=w', y<=h']
          no = [(x,y) | (x,y) <- pts, x>w' , y<=h']
          sw = [(x,y) | (x,y) <- pts, x<=w', y>h']
          so = [(x,y) | (x,y) <- pts, x>w' , y>h']
          w' = truncate $ fromIntegral w/2
          h' = truncate $ fromIntegral h/2
          ds' ls = snd $ distSort ls
          
          ds ls = L.reverse $ snd $ distSort ls

starSortNODS :: Int -> Int -> [Point] -> [Point]
starSortNODS w h []  = []
starSortNODS w h pts = (  
                       ds                      [(x,y) | (x,y) <- nw, x>y   ]
                    ++ ds (L.reverse $ sortByY [(x,y) | (x,y) <- no, x<=h-y]) -- Maybe needs sortByY
                    ++ ds (L.reverse           [(x,y) | (x,y) <- no, x>h-y ]) -- Maybe needs L.reverse
                    ++ ds                      [(x,y) | (x,y) <- so, x>y   ]
                    ++ ds (L.reverse           [(x,y) | (x,y) <- so, x<=y  ]) -- Maybe needs L.reverse
                    ++ ds (L.reverse           [(x,y) | (x,y) <- sw, x>h-y ]) -- Maybe needs L.reverse
                    ++ ds (L.reverse $ sortByY [(x,y) | (x,y) <- sw, x<=h-y]) -- Maybe needs sortByY
                    ++ ds (L.reverse           [(x,y) | (x,y) <- nw, x<=y  ])
                    ) 
    where nw = [(x,y) | (x,y) <- pts, x<=w', y<=h']
          no = [(x,y) | (x,y) <- pts, x>w' , y<=h']
          sw = [(x,y) | (x,y) <- pts, x<=w', y>h']
          so = [(x,y) | (x,y) <- pts, x>w' , y>h']
          w' = truncate $ fromIntegral w/2
          h' = truncate $ fromIntegral h/2
          ds ls = ls 

starSortPar :: Int -> Int -> [Point] -> [Point]
starSortPar w h [] = []
starSortPar w h pts = runEval $ do
                             let  nw = [(x,y) | (x,y) <- pts, x<=w', y<=h']
                                  no = [(x,y) | (x,y) <- pts, x>w' , y<=h']
                                  sw = [(x,y) | (x,y) <- pts, x<=w', y>h']
                                  so = [(x,y) | (x,y) <- pts, x>w' , y>h']
                                  w' = truncate $ fromIntegral w/2
                                  h' = truncate $ fromIntegral h/2
                                  ds ls = L.reverse $ snd $ distSort ls 
                             -- parMap1 (ds )
                             l1 <- rpar $ ds                      [(x,y) | (x,y) <- nw, x>y ]
                             l2 <- rpar $ ds (L.reverse $ sortByY [(x,y) | (x,y) <- no, x<=h-y])
                             l3 <- rpar $ ds (L.reverse           [(x,y) | (x,y) <- no, x>h-y ])
                             l4 <- rpar $ ds                      [(x,y) | (x,y) <- so, x>y   ]
                             l5 <- rpar $ ds (L.reverse           [(x,y) | (x,y) <- so, x<=y  ])
                             l6 <- rpar $ ds (L.reverse           [(x,y) | (x,y) <- sw, x>h-y ])
                             l7 <- rpar $ ds (L.reverse $ sortByY [(x,y) | (x,y) <- sw, x<=h-y])
                             l8 <- rpar $ ds (L.reverse           [(x,y) | (x,y) <- nw, x<=y  ])
                             rseq l1
                             rseq l2
                             rseq l3
                             rseq l4
                             rseq l5
                             rseq l6
                             rseq l7
                             rseq l8
                             return (l1++l2++l3++l4++l5++l6++l7++l8)


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


-- generates a Heatmap Image of a [Point]
pointlsToImg :: [Point] -> Int -> Int -> Image PixelRGB8
pointlsToImg ls w h = Image { imageWidth = w, imageHeight = h, imageData = generated }
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
            drawHeatMap w arr nul ls -- for debugging
            V.unsafeFreeze arr
              where
              nul = 0 :: Int
              --draws a "HeatMap" to see how the toolpath is progressing
              drawHeatMap w arr _     []     = return ()
              drawHeatMap w arr count ((x,y):ps) = do unsafeWritePixel arr p' (PixelRGB8 cr cg cb)
                                                      drawHeatMap w arr count' ps
                                                  where p' = (x+y*w)*3
                                                        count' = if count < 765
                                                                  then count+1
                                                                  else count-765
                                                        (cr,cg,cb) = if count < 255
                                                                      then ((fromIntegral count),0,0)
                                                                      else if count < 510
                                                                            then (0,(fromIntegral count),0)
                                                                            else if count < 765
                                                                                  then (0,0,(fromIntegral count))
                                                                                  else (0,255,255)

























