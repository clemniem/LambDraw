module Gui.LambGui where

import Core.MakeIMG
import Core.ProcessImage

import Control.Monad
import Safe 
import Data.Ratio
import System.Directory
import System.FilePath.Posix
import Data.Maybe (fromJust)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Codec.Picture.Types
import System.Random as R



-- | DIN A4 210 x 297 mm
drawMax :: Point
drawMax = (210,297) -- (width,height)

-- | helpers for setting CSS style
stfont,stva,padding :: UI Element -> UI Element
stfont  = set style [("font-size","12px")]
stva    = set style [("vertical-align","top")]
padding = set style [("padding","10px 10px 10px 10px")]

-- | Bound Checking Helper Function
fromMaybeSize :: Maybe (Int,Int) -> (Int,Int)
fromMaybeSize (Just msize) = msize
fromMaybeSize _           = (0,0)

-- | getter for specific Color () of a Canvas at a specific Point
-- to UI (PixelRGB8) is also possible just change from fst to snd after the return
getCanvCol :: UI.Canvas -> UI.Point -> UI (PixelRGB8) 
getCanvCol canvas (x,y) = do  
  str <- callFunction $ ffi ("(%1.getContext('2d').getImageData(%2,%3,1,1).data[0])+\
                            \\",\"+(%1.getContext('2d').getImageData(%2,%3,1,1).data[1])+\
                            \\",\"+(%1.getContext('2d').getImageData(%2,%3,1,1).data[2])") 
                            canvas x y
  return $ tripleToCol $ lsToRGB $ wordsWhen (==',') str
   where
   -- could also use splitOn
   wordsWhen     :: (Char -> Bool) -> String -> [String]
   wordsWhen p s =  case dropWhile p s of
                         "" -> []
                         s' -> w : wordsWhen p s''
                               where (w, s'') = break p s'
   -- take a list of strings and make a triple of ints 
   lsToRGB :: [String] -> (Maybe Int,Maybe Int,Maybe Int)
   lsToRGB (a:b:c:_xs) = (readMay a, readMay b, readMay c)
   lsToRGB _           = (Just 255, Just 255, Just 255) 
   -- make a triple of Int to Color needed
   tripleToCol :: (Maybe Int,Maybe Int,Maybe Int) -> PixelRGB8
   tripleToCol (r,g,b) = safeColorRGB8 r g b

-- | safe Conversion to Color 
safeColorUI :: Maybe Int -> Maybe Int -> Maybe Int -> UI.Color 
safeColorUI mr mg mb = UI.RGB (tst mr) (tst mg) (tst mb)
    where tst Nothing  = 0
          tst (Just i)
            | i > 255   = 255
            | i < 0     = 0
            | otherwise = i

-- | Safe Conversion to PixelRGB8
safeColorRGB8 :: Maybe Int -> Maybe Int -> Maybe Int -> PixelRGB8 
safeColorRGB8 mr mg mb = PixelRGB8 (tst mr) (tst mg) (tst mb)
    where tst Nothing  = 0
          tst (Just i)
            | i > 255   = 255
            | i < 0     = 0
            | otherwise = fromIntegral i

-- | Safe Conversion String -> Int
safeTupel :: (String,String) -> (Int,Int)
safeTupel (str1,str2) = (toInt (readMay str1),toInt (readMay str2))
  where 
    toInt :: Maybe Int -> Int
    toInt (Just nr) = nr
    toInt _         = 1  

-- | get a random three Digits Number String
getRand :: [Char] -> [Char]
getRand char = take 3 (randomRs ('0', '9') gen)
    where gen = (mkStdGen (read char))


{-----------------------------------------------------------------------------
    Gui-Main
------------------------------------------------------------------------------}

startGui :: IO ()
startGui = startGUI defaultConfig { 
            tpStatic     = Just "./images"
            } setup

{-----------------------------------------------------------------------------
    Setup
------------------------------------------------------------------------------}

setup :: Window -> UI ()
setup window = do
    return window # set title "LambDraw"
    UI.addStyleSheet window "LambGui.css"
    
    statURLOut <- return "static/temp/temp"
    ---------------------- SETUP --------------------------
    -- helper Divs needed for calculation 
    elURLin   <- UI.new 
    elURLout  <- UI.new  
    elURLdith <- UI.new 
    elURLres  <- UI.new 
    elRand    <- UI.new # set UI.value "0"
    elcanvWidth <- UI.new 

    ---------------------- LOADIMG -------------------------
    elDW <- UI.input # set UI.value (show $ fst drawMax)
    elDH <- UI.input # set UI.value (show $ snd drawMax)

    elIpathIn <- UI.input
    elBload   <- UI.button #+ [string "Load PNG"]

    elDload   <- UI.div #+ [row [element elIpathIn,
                                 element elBload
                            ]       ] 
    elDimgs     <- UI.div
    elIimgOrig  <- UI.image
        # set UI.height 0
        # set UI.width  0
        # set style [("border", "solid black 1px")]
        -- # set UI.src "static/canvas.png"

    ---------------------- RESIZE --------------------------
    elIimgWidth    <- UI.span # set UI.text "---" -- img w
                              # padding
    elIimgHeight   <- UI.span # set UI.text "---"-- img h    
                              # padding
    elIoutWidth   <- UI.input -- dw
    elIoutHeight  <- UI.input -- dh    
        


    elresFac       <- UI.new
    
    elBapplyResize <- UI.button #+ [string "Apply Resize"]
    elBlsresize    <- UI.div #+ [element elBapplyResize, element elresFac]

    elDresize      <- UI.div
    elDresizeInv   <- UI.div #+ [ grid [[string "",    string "Machine:",string "  .png: "   ,string "Output:"],
                                        [string "Width: " , element elDW,element elIimgWidth ,element elIoutWidth ],
                                        [string "Height: ", element elDH,element elIimgHeight,element elIoutHeight]] # set style [("padding","3px 5px 3px")],
                                row [element elBlsresize] # padding]
                            
    ---------------------- COLOR PICKER --------------------
    [elrVal,elgVal,elbVal] <- mapM (UI.input #+) [[string "0"],[string "0"],[string "0"]]
    [addCol1,addCol2,addCol3,addCol4] <- mapM (UI.button #+) [[string "A"],[string "B"],[string "C"],[string "D"]]

    elBclearPal <- UI.button #+ [string "Clear Pallette"]
    removeColor <- UI.button #+ [string "Remove Color"]

    canvas <- UI.canvas
        # set UI.height 35
        # set UI.width  35
        # set style [("border", "solid black 1px")]
    palCanvas <- UI.canvas
        # set UI.height 35
        # set UI.width  (35*4)
        # set style [("border", "solid black 1px")]
        # set UI.fillStyle (UI.solidColor $ UI.RGB 255 255 255)
    UI.fillRect (0,0) (35*4) (35) palCanvas

    elDithGen <- UI.input # set UI.value "3" 
                          # set UI.text "3"
                          # set UI.size "3"

    colPick <- UI.div #+ [row [column [grid [[string "R",element elrVal], [string "G", element elgVal],[string "B", element elbVal]]  # set style [("padding","0px 0px 5px")]
                                            ,row [element canvas # stva
                                                 ,column [row [element addCol1
                                                               ,element addCol2
                                                               ,element addCol3
                                                               ,element addCol4] # stva  
                                                         ,element removeColor] # set style [("padding","0px 0px 0px 5px")]
                                                 ]] # stva # padding
                              ,column [element palCanvas # stva 
                                      ,element elBclearPal
                                      ,row [string "Dither Strength (0-3)" # stfont
                                           ,element elDithGen]]] 
                         ]
        # set UI.height 300
        # set UI.width  300
        # set style [("left", "50 px")]
        # set UI.align "top"
        # set UI.valign "left"
    ---------------------- DITHER --------------------------


    elBapplyDither <- UI.button #+ [string "Apply Dither."]
    elDdither      <- UI.div #+ [element elBapplyDither]    

    ---------------------- COLORSPLICER --------------------
    elBsplice <- UI.button #+ [string "Apply Splice"]
    elDsplice <- UI.div #+ [element elBsplice]
 
    ---------------------- BODY ----------------------------
    getBody window #+ [ 
        string "Welcome to LambDraw 0.42" # set style [ ("font-family","Gill Sans, Verdana"),
                                                        ("font-size","11px"),
                                                        ("line-height","14px"),
                                                        ("text-transform","uppercase"),
                                                        ("letter-spacing","2px"),("font-weight","bold")],
        element elDload # padding,
        element elDresize,
        element colPick,
        row [element elDdither,element elDsplice] # padding,
        row [element elDimgs]
        
        ]

{-----------------------------------------------------------------------------
                        Start GUI
    ------------------------------------------------------------------------------}

    -------GUI--------------- LOADIMG -------------------------
    bUrlIn <- stepper "" $ UI.valueChange elIpathIn
    
    let updateRand = do [oldRand] <- getValuesList [elRand]
                        let newRand = getRand oldRand
                        element elRand # set UI.value newRand

    let refreshImage url (Just w) = do element elIimgOrig # set UI.height w # set UI.src url

    -- reloads size inputs and calculates new ratio etc.
    let reloadSize nosave = do  
            [strdW,strdH,
             strWidth,strHeight,
             strOutW,strOutH] <- getValuesList [elDW,elDH,elIimgWidth,elIimgHeight,elIoutWidth,elIoutHeight]
            let dwh@(_draW ,_drawH) = safeTupel (strdW,strdH)
                size@(width,height) = safeTupel (strWidth,strHeight)
                out@(outW,outH)     = safeTupel (strOutW,strOutH) 
                rat@(_h_rat,_w_rat) = funTupel (%) (funTupel min dwh out) size  
                ratio = min (1%1) $ uncurry min rat
            liftIOLater $ do putStrLn ("Machine : "++show dwh)
                             putStrLn ("Size : " ++ show size)
                             putStrLn ("OutSize: "++ show out)
                             putStrLn ("Ratios: "++show rat)
                             putStrLn ("Ratio: "++show ratio)
            if nosave 
              then liftIOLater $ putStrLn "No Saving.."
              else do [urlIn,urlOut,rand,strcanW] <- getValuesList [elURLin,elURLout,elRand,elcanvWidth]
                      let newSize@(newH,_newW) = funTupel (*) (ratio,ratio) (fromIntegral width,fromIntegral height)
                          mcW = readMay strcanW 
                          canW = if (mcW == Nothing) then (Just 300) else mcW  
                      liftIO $ preProcessImage (doResize $ round newH) [] urlIn (urlOut++rand)

                      refreshImage (statURLOut++rand++"_res.png") canW 
                      element elURLres   # set UI.value (urlOut++rand++"_res.png")
                      updateRand
                      liftIOLater $ do 
                            print $ "Resize URL IN : "++urlIn
                            print $ "Resize URL OUT : "++(urlOut++rand++"_res.png")
                            mapM_ print $ show rat:(map show [size,dwh])++[show newSize] 

    -- reads Image size and applys to Canvas
    let readSize = do 
            urlIn   <- currentValue bUrlIn
            ioMsize <- liftIO $ return $ getImgSize urlIn
            mSize   <- liftIO ioMsize
            let size@(width,height) = fromMaybeSize mSize
                (strWidth,strHeight) = (show width,show height)
                res2canv = fromIntegral $ round $ (300%height) * (fromIntegral width)
            element elIimgOrig # set UI.height 300
            forM [element elIimgOrig,element elcanvWidth]    (# set UI.width res2canv )
            forM [element elIimgWidth,element elIoutWidth]   (# set UI.value strWidth )
            forM [element elIimgHeight,element elIoutHeight] (# set UI.value strHeight)
            element elIimgWidth   # set UI.text  strWidth
            element elIimgHeight  # set UI.text  strHeight
            reloadSize True
            liftIOLater $ print size

    -- loads image and updates variables
    on UI.click elBload $ const $ do urlIn <- currentValue bUrlIn
                                     ioexists <- liftIO $ return $ doesFileExist urlIn
                                     exists <- liftIO ioexists
                                     ispng  <- return $ takeExtension urlIn == ".png" 
                                     if not $ and [exists,ispng] 
                                        then do liftIO $ putStrLn "Wrong Filepath... can't find a .png file"
                                        else do   
                                         dir <- return $ dropFileName urlIn
                                         let tempDir = (dir++"temp/")
                                         liftIOLater $ createDirectoryIfMissing False tempDir
                                         forM [element elURLin, element elURLdith] (# set UI.value urlIn)
                                         element elURLres # set UI.value urlIn
                                         element elURLout # set UI.value (tempDir++"temp")
                                         uri <- loadFile "image" urlIn -- "image" = MIME Type
                                         readSize  
                                         element elIimgOrig # set UI.src uri
                                         element elDimgs #+ [element elIimgOrig]
                                         element elDresize #+ [element elDresizeInv]
                                         liftIO $ putStrLn "Image loaded"
                                     

    -------GUI--------------- RESIZE --------------------------

    --forM (map 
    on UI.valueChange elIoutWidth  $ return $ reloadSize True
    on UI.valueChange elIoutHeight $ return $ reloadSize True                                 
    on UI.valueChange elDW $ return $ reloadSize True                                 
    on UI.valueChange elDH $ return $ reloadSize True                                 
    on UI.click elBapplyResize $ return $ do reloadSize False
 
    -------GUI--------------- COLOR PICKER --------------------
    -- update Values in Color Picker
    [bRIn,bGIn,bBIn,dithIn] <- mapM (stepper "0") $ map UI.valueChange [elrVal,elgVal,elbVal,elDithGen]
    
    -- function to update a canvas
    let updateCanv canv nr = const $ do
                                [rIn,gIn,bIn]  <- mapM currentValue [bRIn,bGIn,bBIn]
                                let (r',g',b')  = (readMay rIn,readMay gIn,readMay bIn)
                                let col@(UI.RGB r g b) =  safeColorUI r' g' b'
                                element canv # set UI.fillStyle (UI.solidColor col)
                                -- immediate Update of Inputs if >255 or <0
                                element elrVal # set UI.value (show r)
                                element elgVal # set UI.value (show g)
                                element elbVal # set UI.value (show b)        
                                UI.fillRect ((nr*35),0) 35 35 canv

    let checkDithValue = const $ do val <- currentValue dithIn
                                    let mval = readMay val
                                    if mval == Nothing
                                        then element elDithGen # set UI.value "3"
                                        else do let newVal = show (max 0 (min 6 (fromJust mval)))
                                                element elDithGen # set UI.value newVal # set UI.text newVal

    -- How can I make this smaller? forM doesnt work...
    

    on UI.valueChange elDithGen $ updateCanv canvas 0

    on UI.valueChange elrVal $ updateCanv canvas 0
    on UI.valueChange elgVal $ updateCanv canvas 0
    on UI.valueChange elbVal $ updateCanv canvas 0
    -- How can I make this smaller?
    on UI.click addCol1 $ updateCanv palCanvas 0
    on UI.click addCol2 $ updateCanv palCanvas 1
    on UI.click addCol3 $ updateCanv palCanvas 2
    on UI.click addCol4 $ updateCanv palCanvas 3

    on UI.click removeColor $ return $ do UI.clearCanvas canvas
                                          forM (map element [elrVal,elgVal,elbVal]) (# set UI.value "0")
    
    let getPallette = mapM (getCanvCol palCanvas) [(1,1),(36,1),(72,1),(108,1)]
    
    on UI.click elBclearPal $ const $ do UI.clearCanvas palCanvas

    -------GUI--------------- DITHER --------------------------    
    on UI.click elBapplyDither $ return $ do [urlIn,urlOut,rand,strcanW,dstrength] <- getValuesList [elURLres,elURLout,elRand,elcanvWidth,elDithGen]                                                         
                                             pal   <- getPallette
                                             liftIO $ do 
                                                print $ pal
                                                print $ "Dither URL IN: "++urlIn
                                                preProcessImage (doTestDither dstrength) (whitePix:pal) urlIn (urlOut++rand)
                                             let mcW = readMay strcanW 
                                                 canW = if (mcW == Nothing) then (Just 300) else mcW  
                                             refreshImage (statURLOut++rand++"_dith.png") canW 
                                             element elURLdith   # set UI.value (urlOut++rand++"_dith.png")
                                             updateRand
                                             liftIO $ print $ "Dither URL Out: "++(urlOut++rand++"_dith.png")

    -------GUI--------------- SPLICE --------------------------    

    on UI.click elBsplice $ return $ do [urlIn, urlOut,dstrength] <- getValuesList [elURLdith,elURLout,elDithGen]
                                        liftIO $ print urlIn
                                        pal   <- getPallette
                                        liftIO $ processImage dstrength (whitePix:pal) urlIn urlOut











