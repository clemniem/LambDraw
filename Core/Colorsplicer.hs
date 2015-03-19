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
import Data.Ratio

-------------------------------------------------------------------------------
----            Helper Functions (Debugging...)
-------------------------------------------------------------------------------
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

lengthStarls = lsDistance starls

smallList = [(x,y) | x <- [0,2..40], y<-[0..20]] :: [Point]

-------------------------------------------------------------------------------
----            Splicing
-------------------------------------------------------------------------------

-- | checks for: Pixel == Pixel 
checkColor :: PixelRGB8 -> PixelRGB8 -> Bool
checkColor (PixelRGB8 a1 a2 a3) (PixelRGB8 b1 b2 b3)
    | a1 /= b1  = False
    | a2 /= b2  = False 
    | a3 /= b3  = False
    | otherwise = True

-- | creates List of Points for one Color from Image PixelRGB8
colorSplicer :: Image PixelRGB8 -> PixelRGB8 -> [Point] 
colorSplicer img (PixelRGB8 255 255 255) = []
colorSplicer img@(Image { imageWidth  = w, imageHeight = h }) pix =
    [(x,y)| x <- [0..w-1], y <-[0..h-1], checkColor (pixelAt img x y) pix]


-------------------------------------------------------------------------------
----            Toolpath Sorting
-------------------------------------------------------------------------------
-- | calculates euclidian distance between two Points.
distance:: Point -> Point -> Distance
distance (x1,y1) (x2,y2) = sqrt $ fromIntegral $ (x1-x2)^2 + (y1-y2)^2

-- | calculates the overall Distance of a [Point] starting at the head
lsDistance :: [Point] -> Distance
lsDistance []       = 0
lsDistance (a:[])   = 0
lsDistance (a:b:cs) = distance a b + lsDistance (b:cs)

-- | sorts a List of Points by their Y Coordinate
sortByY :: [Point] -> [Point]
sortByY = sortBy (comparing snd)

-- | sorts a List of Points by their X Coordinate
sortByX :: [Point] -> [Point]
sortByX = sort

-- | Takes a List of Points, starts with the head and sorts by the closest Point
-- returns Distance and List
distSort :: [Point] -> (Maybe Distance,[Point])
distSort [] = (Nothing,[])
distSort (x:xs) = distSortAcc (nextPoint x xs) (Just 0,[x])
    where
    distSortAcc :: (Maybe Point,Distance,[Point]) -> (Maybe Distance,[Point]) -> (Maybe Distance,[Point]) 
    distSortAcc  (Nothing,_,[]) acc = acc
    distSortAcc  (Just p,dist,xs) (distAcc,lsAcc) = distSortAcc (nextPoint p xs) (fmap (+dist) distAcc,p:lsAcc)
    -- calculates the closest Point from a list to a given Point. 
    -- Returns: (closest Point, distance, rest of the list)
    nextPoint :: Point -> [Point] -> (Maybe Point,Distance,[Point])
    nextPoint p [] = (Nothing,0,[])
    nextPoint p origls@(x:xs)
        | elem p origls = nextPoint p $ L.delete p origls
        | otherwise = nextPointAcc 1500 (distance p x) (p,x) xs
        where
        nextPointAcc :: Int -> Distance -> (Point,Point) -> [Point] -> (Maybe Point,Distance,[Point])
        nextPointAcc 0 acc (p,np) _ = (Just np, acc ,delete np origls) 
        nextPointAcc c acc (p,np) [] = (Just np, acc ,delete np origls) 
        nextPointAcc c acc (p,np) (x:xs)
            | distance p x < acc = nextPointAcc (c-1) (distance p x) (p,x) xs
            | otherwise = nextPointAcc (c-1) acc (p,np) xs

-- | Helper Function to use the sorted List from distSort
dSort::[Point] -> [Point]
dSort ls = L.reverse $ snd $ distSort ls  

-- | attempt to beat my shabby looking distSort
fooSort :: [Point] -> [Point]
fooSort []           = []
fooSort [pa]         = [pa]
fooSort origls@(pa:pb:ps) = pa : nepo (pa,pb) ps (distance pa pb)
  where
  pres = pb:ps 
  nepo :: (Point,Point) -> [Point] -> Distance -> [Point]
  nepo tup@(lp,pcur@(x_curr,y_curr)) [] dist = fooSort (pcur :(L.delete pcur pres))
  nepo tup@(lp,pcur@(x_curr,y_curr)) (pn@(xn,yn):pps) dist
    | fromIntegral (abs (x_curr-xn)) > dist  = nepo tup pps dist   
    | dist' >= dist = nepo tup pps dist
    | otherwise    = nepo (lp,pn) pps dist'
      where dist' = distance lp pn

-- | parallel attempt to beat my shabby looking distSort
fooSortPar :: [Point] -> [Point]
fooSortPar []           = []
fooSortPar [pa]         = [pa]
fooSortPar origls@(pa:pb:ps) = pa : nepoPar pa pres
  where
  pres = pb:ps 
  nepoPar :: Point -> [Point] -> [Point]
  nepoPar pp []   = []
  nepoPar pp [p1] = [p1]
  nepoPar pp pps  = fooSortPar (nexp : (L.delete nexp pres))
    where nexp = snd $ head $ sort $ runEval $ do parMap1 (brum pp) parls
          leng  = length pps
          parls = (take 300 pps)
          brum :: Point -> Point -> (Distance,Point)
          brum a b = (distance a b, b)
          
{- 
Short Overview: Stats

foo vs fooPar vs distSort
500x500        mm           time
foo            101026.48   1.23s
fooPar[20]     289257.28   0.08s
distSort       100134.82   2.84s
fooPar[100]    163754.72   0.16s
fooPar[200]    129470.12   0.57s
fooPar[500]    110460.79   2.88s
fooPar[100]    163754.72   0.16s
distSort[1500] 103152.83   0.66s -}



{- | STARSORT *
-------------------------------------------------------------------------------
\n
.\.1|2./..\n
.8\.|./3..\n
----c-----\n
..7/.|.\4.\n
../6.|5.\.\n

-------------------------------------------------------------------------------
-- Divide Image (Grid) into 8 Slices (with list comprehension) and Sort them (D&C) 
-}
-----------------------------------------------------------------------------}

starSort ::    Int     -- ^ width of Grid
            -> Int     -- ^ height of Grid
            -> [Point] -- ^ list of Points you want to sort 
            -> [Point] -- ^ sorted List of Points
starSort w h []  = []
starSort w h pts = (  
                       ds                      [(x,y) | (x,y) <- nw, x>y   ]
                    ++ ds                      [(x,y) | (x,y) <- no, x<=h-y] 
                    ++ ds (L.reverse           [(x,y) | (x,y) <- no, x>h-y ]) 
                    ++ ds                      [(x,y) | (x,y) <- so, x>y   ]
                    ++ ds (L.reverse           [(x,y) | (x,y) <- so, x<=y  ]) 
                    ++ ds (L.reverse           [(x,y) | (x,y) <- sw, x>h-y ]) 
                    ++ ds (L.reverse $ sortByY [(x,y) | (x,y) <- sw, x<=h-y]) 
                    ++ ds (L.reverse           [(x,y) | (x,y) <- nw, x<=y  ])
                    ) 
    where nw = [(x,y) | (x,y) <- pts, x<=w', y<=h']
          no = [(x,y) | (x,y) <- pts, x>w' , y<=h']
          sw = [(x,y) | (x,y) <- pts, x<=w', y>h']
          so = [(x,y) | (x,y) <- pts, x>w' , y>h']
          w' = truncate $ fromIntegral w/2
          h' = truncate $ fromIntegral h/2
          ds' ls = snd $ distSort ls
          --ds ls = fooSort ls
          --ds ls = fooSortPar ls
          ds ls = dSort ls


{- | attempt to parallelise it: 
300x300

Seq:        39978mm     6.09s

Par:        39978mm    11.59s  
-}
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


-- | Calculates the sorted List of closest Points for every Point in the List 
--   returns the one with minimal Distance
distOptimizer :: [Point] -> (Maybe Float,[Point])
distOptimizer [] = (Nothing, [])
distOptimizer ls@(x:xs) = distOptimizerAcc ls xs $ distSort ls
    where
    distOptimizerAcc :: [Point] -> [Point] -> (Maybe Float,[Point]) -> (Maybe Float,[Point])
    distOptimizerAcc origls [] acc = acc
    distOptimizerAcc origls countls@(x:xs) acc
        | fst (distSort (x:origls)) < fst acc = distOptimizerAcc origls xs (distSort (x:origls))
        | otherwise = distOptimizerAcc origls xs acc


-- | generates a Heatmap Image of a [Point]
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
            --draw a white pic
            lineGenerator 0 0
            drawHeatMap w arr 0 ls
            V.unsafeFreeze arr
              where
              --draws a "HeatMap" to see how the toolpath is progressing
              drawHeatMap w arr _     []     = return ()
              drawHeatMap w arr count ((x,y):ps) = do unsafeWritePixel arr p' $ heat count
                                                      drawHeatMap w arr (count+1) ps
                                                  where p' = (x+y*w)*3
                                                        maxcount = length ls
                                                        heat :: Int -> PixelRGB8
                                                        heat cur = PixelRGB8 r g b
                                                          where proz = round $ (cur%maxcount)*100
                                                                r = if proz < 50 then round $ proz     *255%50 else 255
                                                                g = if proz > 50 then round $ (proz-50)*255%50 else 0 
                                                                b = 0--if proz < 66 then 0   else round $ (255%3) * (proz-66)

























