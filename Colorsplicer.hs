module Colorsplicer where

import Dither
import MakeIMG

import Codec.Picture.Types
import Data.List as L
import Data.Ord

dynDith = ImageRGB8 picDith

picDith = ditherFloydRGB8 rgbPixls pic

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

-- helper Functions for ghci
lsAccessor :: (Maybe Point,Float,[Point]) -> [Point]
lsAccessor (_,_,xs) = xs

pAccessor :: (Maybe Point,Float,[Point]) -> Point
pAccessor (Just p,_,_) = p
pAccessor (Nothing,_,_) = (-1,-1)

flAccessor :: (Maybe Point,Float,[Point]) -> Float
flAccessor (_,f,_) = f

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



































