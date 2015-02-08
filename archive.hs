{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
import Data.List as L
import qualified Data.ByteString.Lazy as BS
import qualified Data.Foldable as Fold
import Data.Ord
import qualified Data.Sequence as Seq
import Data.Word
import Data.Bits
import System.Environment
import Codec.Picture
import Codec.Picture.Types
import Data.Array
import ErrDither

-- HelperVariables
distMax = distance (0,0) (drawWidth,drawHeight)


-- Type Declarations
--type Point = (Int,Int)
--type Distance = Float


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




--------------------------------------------------------------
-- Color Pixel Code --
type Accessor = PixelRGB8 -> Pixel8

data ValRGB = ValRGB8 (Pixel8,Pixel8,Pixel8) | ValRGB16 (Pixel16,Pixel16,Pixel16)  
	deriving (Eq, Show) 

data ColorVal = CV1 | CV2 | CV3 | CV4 | CVN 
    deriving (Ord, Eq, Show)

data ColorVal8 = CVOne PixelRGB8 | CVTwo PixelRGB8 | CVThree PixelRGB8 | CVFour PixelRGB8 | CVNope PixelRGB8
	deriving (Ord, Eq, Show)

type CVPalette = [ColorVal8]

cv8toPix :: ColorVal8 -> PixelRGB8
cv8toPix (CVOne   p) = p
cv8toPix (CVTwo   p) = p
cv8toPix (CVThree p) = p
cv8toPix (CVFour  p) = p
cv8toPix (CVNope  p) = p

cv8toCV :: ColorVal8 -> ColorVal
cv8toCV (CVOne   _) = CV1
cv8toCV (CVTwo   _) = CV2
cv8toCV (CVThree _) = CV3
cv8toCV (CVFour  _) = CV4
cv8toCV _           = CVN

cvtoCV8 :: ColorVal -> ColorVal8
cvtoCV8 CV1 = head varCVs
cvtoCV8 CV2 = head $ drop 1 varCVs
cvtoCV8 CV3 = head $ drop 2 varCVs
cvtoCV8 CV4 = head $ drop 3 varCVs
cvtoCV8 CVN = head $ drop 4 varCVs

-- -- Getters for pixel components, as the constructor does not
-- -- provide any public ones.
-- red, blue, green :: Accessor
-- red   (PixelRGB8 r _ _) = r
-- green (PixelRGB8 _ g _) = g
-- blue  (PixelRGB8 _ _ b) = b

-- -- HelperVariables for Testing
-- redPix   = PixelRGB8 255 0   0
-- greenPix = PixelRGB8 0   255 0
-- bluePix  = PixelRGB8 0   0   255
-- blackPix = PixelRGB8 0   0   0
-- whitePix = PixelRGB8 255 255 255
-- greyPix  = PixelRGB8 188 188 188

listPix      = [redPix,greenPix,bluePix,blackPix,whitePix]
testListPix  = [PixelRGB8 124 124 124,PixelRGB8 200 30 210,PixelRGB8 0 0 0,PixelRGB8 200 100 100,PixelRGB8 100 100 130,PixelRGB8 255 255 255,PixelRGB8 25 150 0]
testListPixS = [PixelRGB8 30 124 124,PixelRGB8 200 30 210,PixelRGB8 0 0 0]

-- Helper Functions
-- encodes a Word16 to Word8
enc16to8 :: Word16 -> Word8
enc16to8 x = fromIntegral x

-- pixTo8 :: (PixelBaseComponent a) => Pixel a -> PixelRGB8
-- pixTo8 p@(PixelRGBA8 _ _ _ _) = dropTransparency p
-- pixTo8 p@(PixelYCbCr8 _ _ _) = dropTransparency


-- valRGB16to8 :: PixelRGB16 -> PixelRGB8
-- valRGB16to8 p = colorMap (fromIntegral) p

compPix :: [PixelRGB8] -> PixelRGB8 -> PixelRGB8 -> Bool
comPix [] _ _ = False
compPix  ls p q = comPixAcc (delete p ls) p q $ colorDist8 p q
	where
	comPixAcc :: [PixelRGB8] -> PixelRGB8 -> PixelRGB8 -> Distance -> Bool
	comPixAcc [] _ _ _ = True
	comPixAcc (x:xs) p q dist
		| dist > (colorDist8 x q) = False
		| otherwise = comPixAcc xs p q dist

-- colorMinDist :: PixelRGB8 -> [PixelRGB8] -> PixelRGB8
-- colorMinDist p []     = PixelRGB8 255 255 255
-- colorMinDist p (x:xs) = colorMinDistAcc p xs ((colorDist8 p x),x) 
--     where
--     colorMinDistAcc :: PixelRGB8 -> [PixelRGB8] -> (Distance,PixelRGB8) -> PixelRGB8
--     colorMinDistAcc p []     (_,pixAcc)    = pixAcc
--     colorMinDistAcc p (x:xs) (dist,pixAcc)
--         | dist' < dist = colorMinDistAcc p xs (dist',x)
--         | otherwise    = colorMinDistAcc p xs (dist ,pixAcc)
--         where
--         dist' = colorDist8 p x 

-- colorDist8 :: PixelRGB8 -> PixelRGB8 -> Distance
-- colorDist8 (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = sqrt $ (2 + r'/256) * dr^2 + 4 * dg^2 + (2+(255-r'/256)*db^2)
--     where
--     r' = (fromIntegral r1)/2 + (fromIntegral r2)/2
--     dr = fromIntegral r1 - fromIntegral r2
--     dg = fromIntegral g1 - fromIntegral g2
--     db = fromIntegral b1 - fromIntegral b2
-- colorDist8 (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = sqrt $ dr^2 + dg^2 + db^2
--     where
--     r' = (fromIntegral r1)/2 + (fromIntegral r2)/2
--     dr = fromIntegral $ r1 - r2
--     dg = fromIntegral $ g1 - g2
--     db = fromIntegral $ b1 - b2 

-- Get all of the pixels in the image in list form.
-- getPixelsCoord :: Pixel a => [PixelRGB8] -> PixelRGB8 -> Image a -> [Point]
-- getPixelsCoord pxs p image =
--   [ (x,y)
--   | x <- [0..(imageWidth image - 1)]
--   , y <- [0..(imageHeight image - 1)]
--   , compPix pxs p $ convertPix $ pixelAt image x y]

varPixCoords = getPixCoords varImgMax varConF

getPixCoords :: ImgMax -> ConvFactor -> [Point]
getPixCoords (imx,imy) f = [(x,y) | x <- [0,f..imx-1], y <- [0,f..imy-1]]


varPixLss = map arrPix varPixCoords 
varResize = map pixLstoCV varPixLss

varRGBsHelper = map cv8toPix $ map cvtoCV8 varResize

newArr = listArray (0,drawWidth*drawHeight-1) varRGBsHelper 

sumby :: Distance -> [Distance] -> Distance
sumby d xs = sumbyAcc d xs 0
    where
    sumbyAcc :: Distance -> [Distance] -> Distance -> Distance
    sumbyAcc d []     acc = acc
    sumbyAcc d (x:xs) acc
        | x <= d    = sumbyAcc d xs $ acc+x
        | otherwise = sumbyAcc d xs   acc

convertPix :: Pixel a => a -> PixelRGB8
--convertPix (PixelRGB8 a b c) = (PixelRGB8 a b c)
convertPix _ = PixelRGB8 0 0 0

-- Image Handler
loadPng :: FilePath -> IO DynamicImage
loadPng path = do
    temp <- readPng path >>= either error return
    return temp

saveImage :: FilePath -> DynamicImage -> IO ()
saveImage name img  = do
    savePngImage (name ++ ".png") img
    putStrLn "Gespeichert." 


-------------------------------------------------------------
-- Array Pixel Code -----------------------------------------

-- Types
type DrawMax = Point 
type ImgMax = Point
type ConvFactor = Int


-- Setup
drawWidth  = 510 :: Int
drawHeight = 510 :: Int
varDrawMax = (drawHeight,drawWidth)

varMinDist = 1500 :: Distance
-- Help Variables for testing
pic = generateImage pixelunggrey 100 100 
varConF = 2

picArray Image {imageData = arr} = arr 

-- e
-- Perform a componentwise pixel operation.
compwise2 :: (Word8 -> Word8 -> Word8) -> PixelRGB8 -> PixelRGB8 -> PixelRGB8
compwise2 f (PixelRGB8 ra ga ba) (PixelRGB8 rb gb bb) =
  PixelRGB8 (f ra rb) (f ga gb) (f ba bb)

-- Compute the absolute difference of two pixels.
diffPixel :: PixelRGB8 -> PixelRGB8 -> PixelRGB8
diffPixel = compwise2 (\x y -> max x y - min x y)

data PixError = PixeError Int Int Int 
    deriving (Show,Eq,Ord)

type Ferror   = Word8 -> Word8 
-- old-new = error




safeAddWord8 :: Word8 -> Word8 -> Word8
safeAddWord8 x y
    | fromIntegral x + fromIntegral y > 255 = 255
    | otherwise     = x+y 

compwise1 :: (Word8 -> Word8) -> PixelRGB8 -> PixelRGB8
compwise1 f (PixelRGB8 r g b) = PixelRGB8 (f r) (f g) (f b)

multDiv16 :: Word8 -> Word8 -> Word8
multDiv16 fact val = (fact*(fromIntegral val)) `shiftR` 4

-- errorCalc :: PixError -> Int -> PixError
-- errorCalc (r,g,b) fac = (cal r, cal g, cal b)
--     where 
--     cal x = fac*x `shiftR` 4

-- -- | Extract a pixel at a given position, (x, y), the origin
-- -- is assumed to be at the corner top left, positive y to the
-- -- bottom of the image
-- pixelAt :: Image a -> Int -> Int -> a
-- pixelAt image@(Image { imageData = arr }) x y 
--             = PixelRGB8 (arr ! (baseIdx + 0))
--                         (arr ! (baseIdx + 1))
--                         (arr ! (baseIdx + 2))
--     where 
--     baseIdx = pixelBaseIndex image x y

-- -- | Calculate the index for the begining of the pixel
-- pixelBaseIndex :: Image a -> Int -> Int -> Int
-- pixelBaseIndex (Image { imageWidth = w }) x y =
--         (x + y * w) * componentCount (undefined :: a)

newPic = generateImage newPixelung drawWidth drawHeight
newDynPic = ImageRGB8 newPic

dynpic = ImageRGB8 (pic)
varImg8 = getvarImg8 $ blub dynpic
varImgMax = getvarImgMax varImg8

varCVs = [CVOne redPix, CVTwo greenPix, CVThree bluePix, CVFour blackPix, CVNope whitePix]

gletscherPix = PixelRGB8 0 255 255

newPixelung :: Int -> Int -> PixelRGB8
newPixelung x y = newArr ! (y+x*drawHeight)
--    0  1  2  3  4
--
--0   0  1  2  3  4
--1   5  6  6  7  9
--2   10 11 12 13 14
--3   15 16 17 18 19
--4   20 21 22 23 24

pixelunggrey :: Int -> Int -> PixelRGB8
pixelunggrey x y = PixelRGB8 128 128 128

pixelung :: Int -> Int -> PixelRGB8
pixelung x y = PixelRGB8 u v (u*v)
    where
        u = fromIntegral x :: Pixel8
        v = fromIntegral y :: Pixel8

pixelung1 :: Int -> Int -> PixelRGB8
pixelung1 x y = PixelRGB8 u 0 v
    where
        u = fromIntegral x :: Pixel8
        v = fromIntegral y :: Pixel8

pixelungrot :: Int -> Int -> PixelRGB8
pixelungrot x y = PixelRGB8 u 0 0
    where
        u = fromIntegral (255-x*10) :: Pixel8
        v = fromIntegral y :: Pixel8

testPixelung :: Int -> Int -> PixelRGB8
testPixelung x y
    | distance (x,y) (mid,mid) < 1 = PixelRGB8 255 0 0
    | distance (x,y) (mid,mid) < 2 = PixelRGB8 0 255 0
    | distance (x,y) (mid,mid) < 3 = PixelRGB8 0 0 255
    | distance (x,y) (mid,mid) < 4 = PixelRGB8 0 0 0
    | otherwise = PixelRGB8 255 255 255
        where
        mid = 2

pixelLines :: Int -> Int -> PixelRGB8
pixelLines x y 
    | m == 0 = PixelRGB8 u v   v
    | m == 1 = PixelRGB8 v u   v
    | m == 2 = PixelRGB8 v v   u
    | m == 3 = PixelRGB8 v v   v
    | m == 4 = PixelRGB8 u u   u
    | m == 5 = PixelRGB8 u u   v
    | m == 6 = PixelRGB8 v u   u
    | m == 7 = PixelRGB8 u v   u
    | m == 8 = PixelRGB8 u u   255
    | otherwise = PixelRGB8 u 101 u
    where
    u = fromIntegral (x*51) :: Pixel8
    v = fromIntegral (x*10) :: Pixel8
    m = mod y $ fromIntegral 10 


--varConF = convF varImgMax varDrawMax

blub :: DynamicImage -> Maybe (Image PixelRGB8)
blub (ImageRGB8 im) = Just im
blub _ = Nothing

getvarImg8 :: Maybe (Image PixelRGB8) -> Image PixelRGB8
getvarImg8 Nothing    = undefined
getvarImg8 (Just img) = img


convF :: ImgMax -> DrawMax -> Int
convF (imgX,imgY) (drawX,drawY)
    | imgX >= imgY   = truncate $ fromIntegral $ imgX `div` drawX
    | otherwise      = truncate $ fromIntegral $ imgY `div` drawX

getvarImgMax :: Image PixelRGB8 -> ImgMax
getvarImgMax img = (imageWidth img,imageHeight img)    




-- loadArr :: FilePath -> Point -> ConvFactor -> IO ()
-- loadArr path (x,y) f = do
--     dynImg <- loadPng path
--     arr <- return $ arrPix x y f $ blub dynImg
--     print arr

-- Liste von Listen sortieren nach laenge von Liste die nach elementen sortiert in listen gepackt wurde
--sortBy (comparing length) $ group $ sort
allthePixls = [(PixelRGB8 r b g) | r <- [0,51..255], b <- [0,51..255], g <- [0,51..255]]



varmaxColorDist = 4119.6514 :: Distance
--MIT LISTCOMPREHENSION
-- pixLstoCV :: [PixelRGB8] -> ColorVal
-- pixLstoCV pixls = pixlCVmap pixls varCVs (99999999999999999, CV1)
--     where
--     pixlCVmap :: [PixelRGB8] -> [ColorVal8] -> (Distance, ColorVal) -> ColorVal
--     pixlCVmap pxs []     acc      = snd acc
--     pixlCVmap pxs (c:cs) (d,cacc)
--         | dist' <= d = pixlCVmap pxs cs (dist', cv8toCV c)
--         | otherwise  = pixlCVmap pxs cs (d, cacc)
--             where
--             dist' = sumby varMinDist $ map (colorDist8 (cv8toPix c)) pxs
-- OHNE LISTCOMPREHENSION
pixLstoCV :: [PixelRGB8] -> ColorVal
pixLstoCV [] = CVN
pixLstoCV ps = vcLstoVC $ pixLstoCVAcc ps varCVs
--  where    
pixLstoCVAcc :: [PixelRGB8] -> [ColorVal8] -> [ColorVal]
pixLstoCVAcc []     cv8s = []
pixLstoCVAcc (p:ps) cv8s = pixtoCVAcc p cv8s (varmaxColorDist*10,CVN) : pixLstoCVAcc ps cv8s



pixtoCVAcc :: PixelRGB8 -> [ColorVal8] -> (Distance, ColorVal) -> ColorVal
pixtoCVAcc _ []     acc         = snd acc
pixtoCVAcc p (c:cs) acc@(dist,_)
    | dist' <= dist  = pixtoCVAcc p cs (dist',cv8toCV c)
    | otherwise      = pixtoCVAcc p cs acc
        where
        dist' = colorDist8 p $ cv8toPix c

vcLstoVC :: [ColorVal] -> ColorVal
vcLstoVC cvs = countMostCommonCV cvs cvCount 
countMostCommonCV :: [ColorVal] -> CVCount -> ColorVal
countMostCommonCV []       cc = mostCV cc
countMostCommonCV (cv:cvs) cc = countMostCommonCV cvs $ cvCountUp cv cc
cvCountUp :: ColorVal -> CVCount -> CVCount
cvCountUp CV1 (a,b,c,d,e) = (a+1,b,c,d,e)
cvCountUp CV2 (a,b,c,d,e) = (a,b+1,c,d,e)
cvCountUp CV3 (a,b,c,d,e) = (a,b,c+1,d,e)
cvCountUp CV4 (a,b,c,d,e) = (a,b,c,d+1,e)
cvCountUp CVN (a,b,c,d,e) = (a,b,c,d,e+1)
mostCV :: CVCount -> ColorVal
mostCV (a,b,c,d,e)
    | and $ map (a>=) [e,b,c,d] = CV1
    | and $ map (b>=) [a,e,c,d] = CV2
    | and $ map (c>=) [a,b,e,d] = CV3
    | and $ map (d>=) [a,b,c,e] = CV4
    | otherwise                = CVN
cvCount = (0,0,0,0,0) :: CVCount

type CVCount = (Int,Int,Int,Int,Int)

arrPix :: Point ->[PixelRGB8]
arrPix p = pToPix $ imgToPoints p (varConF-1) varImgMax
    where
    imgToPoints :: Point -> ConvFactor -> ImgMax -> [Point]
    imgToPoints (x,y) f (xmax,ymax) = [(x',y')| 
                                            x' <- [x..(x+f)], 
                                            y' <- [y..(y+f)], 
                                            x' <= (xmax-1),
                                            y' <= (ymax-1)]
    pToPix :: [Point] -> [PixelRGB8]
    pToPix []     = []
    pToPix (p:ps) = (uncurry (pixelAt varImg8) p): pToPix ps

toVecPos :: Int -> (Point, PixelRGB8) -> [(Int,Word8)]
toVecPos w ((x,y),(PixelRGB8 r g b)) = [((baseInd + 0),r),((baseInd + 1),g),((baseInd + 2),b)]
  where baseInd = (x + y * w) * componentCount (undefined :: PixelRGB8)

------------------------------------Colorquant-------------------------------

-- addWordError :: Int -> Word8 -> Int -> Word8
-- addWordError fac val err
--             | res <= 0   = 0
--             | res >= 255 = 255
--             | otherwise  = fromIntegral res
--             where res = fromIntegral val + ((fac*(fromIntegral err) `shiftR` 4))

-- addPixError :: Int -> PixelRGB8 -> PixError -> PixelRGB8
-- addPixError fact (PixelRGB8 r g b) (PixError x y z) = PixelRGB8 (pixError r x) (pixError g y) (pixError b z)
--   where 
--   pixError u v
--     | res <= 0   = 0
--     | res >= 255 = 255
--     | otherwise  = fromIntegral res
--     where res = fromIntegral u + ((fact*(fromIntegral v) `shiftR` 4))


-- getPixCoords :: ImgMax -> ConvFactor -> [Point]
-- getPixCoords (imx,imy) f = [(x,y) | x <- [0,f..imx-1], y <- [0,f..imy-1]]


-- getvarImgMax :: Image PixelRGB8 -> ImgMax
-- getvarImgMax img = (imageWidth img,imageHeight img)    

-- imgPls w h = [(x,y)| x <- [0..w-1], y <- [0..h-1]]






