module Gui.LambGui where

import Core.MakeIMG as I
import Core.Colorsplicer
import Core.Dither 
import Core.Resize
import Core.ProcessImage

import Control.Monad
import Safe 
import Data.Ratio

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Codec.Picture.Types

ifSize :: Maybe (Int,Int) -> (Int,Int)
ifSize (Just msize) = msize
ifSize _           = (0,0)

-- DIN A4 210 x 297 mm
drawMax = (210,297) -- (width,height)

stva = set style [("vertical-align","top")]
padding = set style [("padding","10px 10px 10px 10px")]
-- ImgMax -> DrawMax -> Bool
imgTooBig :: (Int,Int) -> (Int,Int) -> Bool
imgTooBig (imW,imH) (dW,dH) = and [imW>dW,imH>dH]

-- getter for specific Color () of a Canvas at a specific Point
-- to UI (PixelRGB8) is also possible just change from fst to snd after the return
getCanvCol :: UI.Canvas -> UI.Point -> UI (PixelRGB8) 
getCanvCol canvas (x,y) = do  
  str <- callFunction $ ffi ("(%1.getContext('2d').getImageData(%2,%3,1,1).data[0])+\
                            \\",\"+(%1.getContext('2d').getImageData(%2,%3,1,1).data[1])+\
                            \\",\"+(%1.getContext('2d').getImageData(%2,%3,1,1).data[2])") 
                            canvas x y
  return $ snd $ tripleToCol $ lsToRGB $ wordsWhen (==',') str
   where
   -- could also use splitOn
   wordsWhen     :: (Char -> Bool) -> String -> [String]
   wordsWhen p s =  case dropWhile p s of
                         "" -> []
                         s' -> w : wordsWhen p s''
                               where (w, s'') = break p s'
   -- take a list of strings and make a triple of ints 
   lsToRGB :: [String] -> (Int,Int,Int)
   lsToRGB (a:b:c:xs) = (read a, read b, read c)
   lsToRGB _          = (255,255,255) 
   -- make a triple of Int to Color needed
   tripleToCol :: (Int,Int,Int) -> (UI.Color, PixelRGB8)
   tripleToCol (r,g,b) = ((UI.RGB r g b),(PixelRGB8 r' g' b'))
     where (r',g',b') = (fromIntegral r,fromIntegral g,fromIntegral b) 

-- | safe Conversion to Color 
safeColorUI :: Maybe Int -> Maybe Int -> Maybe Int -> UI.Color 
safeColorUI mr mg mb = UI.RGB (tst mr) (tst mg) (tst mb)
    where tst Nothing  = 0
          tst (Just i)
            | i > 255   = 255
            | i < 0     = 0
            | otherwise = i

safeColorRGB8 :: Maybe Int -> Maybe Int -> Maybe Int -> PixelRGB8 
safeColorRGB8 mr mg mb = PixelRGB8 (tst mr) (tst mg) (tst mb)
    where tst Nothing  = 0
          tst (Just i)
            | i > 255   = 255
            | i < 0     = 0
            | otherwise = fromIntegral i

safeTupel :: (String,String) -> (Int,Int)
safeTupel (str1,str2) = (toInt (readMay str1),toInt (readMay str2))
  where 
    toInt :: Maybe Int -> Int
    toInt (Just nr) = nr
    toInt _         = 1  

-- used to calculate Neighbour-Points
--funTupel :: (a -> b) -> (a,a) -> (a,a) -> (b,b)
funTupel f (x1,x2) (y1,y2) = (f x1 y1,f x2 y2)

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
    ---------------------- SETUP --------------------------
    elDivHIDE <- UI.div -- used to Hide elements
    elURL     <- UI.div 
    ---------------------- LOADIMG -------------------------
    elDW <- UI.input # set UI.value (show $ fst drawMax)
    elDH <- UI.input # set UI.value (show $ snd drawMax)

    elIpathIn <- UI.input
    elBload   <- UI.button #+ [string "Load PNG"]


    elDload   <- UI.div #+ [row [element elIpathIn,
                                 element elBload,
                                 column [element elDW,
                                         element elDH] # set style [("padding","20px")
                            ]   ]       ] 
    elDimgs   <- UI.div
    elIimgOrig  <- UI.image
        # set UI.height 300
        # set UI.width  300
        # set style [("border", "solid black 1px")]
        # set UI.src "static/canvas.png"

    ---------------------- RESIZE --------------------------
    elIimgWidth    <- UI.span # set UI.text "---" -- img w
                              # padding
    elIimgHeight   <- UI.span # set UI.text "---"-- img h    
                              # padding
    elIdrawWidth   <- UI.input -- dw
    elIdrawHeight  <- UI.input -- dh    
    
    elimgSizeVal   <- UI.input -- (imgW,imgH)
    
    elIimgRes      <- UI.image
        # set style [("border", "solid black 1px")]
        # set UI.src "static/t2.png"
    elBcalcSize    <- UI.button #+ [string "Calc New Size"] 
    
    elresFac       <- UI.new
    
    elBlsresize    <- UI.div #+ [element elBcalcSize, element elresFac]
    elBapplyResize <- UI.button #+ [string "Apply Resize"]
    elDresize      <- UI.div
    elDresizeInv   <- UI.div #+ [string "Image too big, needs Resizing:" # padding, 
                                grid [[string "img Width: ", element elIimgWidth, 
                                        string "dWidth:", element elIdrawWidth  ],
                                      [string "img Height: ",element elIimgHeight, 
                                        string "dHeight:", element elIdrawHeight]],
                                row [element elBlsresize] # padding]
                             # padding
    ---------------------- COLOR PICKER --------------------
    [elrVal,elgVal,elbVal] <- mapM (UI.input #+) [[string "0"],[string "0"],[string "0"]]

    addCol1    <- UI.button #+ [string "A"]
    addCol2    <- UI.button #+ [string "B"]
    addCol3    <- UI.button #+ [string "C"]
    addCol4    <- UI.button #+ [string "D"]

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

    colPick <- UI.div #+ [row [column [grid [[string "R",element elrVal], [string "G", element elgVal],[string "B", element elbVal]]
                         ,row [element canvas # stva, column [row [element addCol1,element addCol2,element addCol3,element addCol4] # stva
                                                      ,element removeColor]]] # stva
                         ,column [element palCanvas # stva , element elBclearPal]]
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
    ---------------------- GCODE ---------------------------
    ---------------------- SAVEFILE ------------------------
    -- ======
--------------------------- BODY ----------------------------
    getBody window #+ [ 
        element elURL,
        element elDload,
        element elDresize,
        row [element elDimgs],
        element elDdither, 
        element colPick,
        element elDsplice
        ]

{-----------------------------------------------------------------------------
                        Start GUI
    ------------------------------------------------------------------------------}


    -------GUI--------------- SETUP ---------------------------

    -------GUI--------------- LOADIMG -------------------------
    bUrlIn <- stepper "./images/piemont.png" $ UI.valueChange elIpathIn
    
    -- | reloads size inputs and calculates new ratio etc.
    let reloadSize = do  
            [strdW,strdH,strWidth,strHeight] <- getValuesList [elDW,elDH,elIimgWidth,elIimgHeight]
            let size@(width,height) = safeTupel (strWidth,strHeight)
                dwh@(draW,drawH)    = safeTupel (strdW,strdH)
                rat@(h_rat,w_rat)   = funTupel (%) dwh size
            element elDresize #+ [element elDresizeInv]
            element elIimgHeight  # set UI.text  strHeight
                                  # set UI.value strHeight
            element elIimgWidth   # set UI.text  strWidth
                                  # set UI.value strWidth
            
            liftIOLater $ do putStrLn ("drawW: "++strdW++" drawH: "++strdH)
                             putStrLn ("Ratio: "++show rat)
            let ratio = uncurry min rat
            element elIdrawWidth  # set UI.value (show $ round (ratio* fromIntegral width))
            element elIdrawHeight # set UI.value (show $ round (ratio* fromIntegral height))

    let readSize = do 
            urlIn   <- currentValue bUrlIn
            ioMsize <- liftIO $ return $ getImgSize urlIn
            mSize   <- liftIO ioMsize
            let size@(width,height) = ifSize mSize
                (strWidth,strHeight) = (show width,show height)
                res2canv = fromIntegral $ round $ (height%300) * (fromIntegral width)
            element elIimgOrig    # set UI.width res2canv
            element elIimgWidth   # set UI.text  strWidth
                                  # set UI.value strWidth                      
            element elIimgHeight  # set UI.text  strHeight
                                  # set UI.value strHeight
            if size > drawMax
              then reloadSize   
              else element elDresize #+ [string "No Resize necessary"]
            liftIOLater $ print size

    on UI.click elBload $ const $ do urlIn <- currentValue bUrlIn
                                     uri <- loadFile "image" urlIn
                                     element elURL # set UI.value urlIn
                                     element elIimgOrig # set UI.src uri
                                     element elDimgs #+ [element elIimgOrig]
                                     readSize  

    -------GUI--------------- RESIZE --------------------------

    [rwIn ,rhIn]  <- mapM (stepper "0") $ map UI.valueChange [elIdrawWidth,elIdrawHeight]

    on UI.click elBcalcSize $ return reloadSize

    --forM_ (map (on UI.valueChange) [elIdrawWidth,elIdrawHeight,elDW,elDH]) (return reloadSize)                                 
    on UI.valueChange elIdrawWidth  $ return reloadSize
    on UI.valueChange elIdrawHeight $ return reloadSize
    on UI.valueChange elDW $ return reloadSize
    on UI.valueChange elDH $ return reloadSize


    on UI.click elBapplyResize $ return $ do urlIn <- currentValue bUrlIn
                                             [balub] <- getValuesList [elresFac]
                                             let [rw,rh] = words balub
                                             let (rh',rw')   = safeTupel (rh,rw)
                                             liftIO $ goResize urlIn "./images/tempRes.png" rh'
                                             element elIimgRes  # set UI.src "static/tempRes.png"
                                                                # set UI.height rh'
                                                                # set UI.width  rw'
                                             element elDimgs #+ [element elIimgRes]
                                             element elURL   # set UI.value "./images/tempRes.png"
                                             liftIOLater $ print rh' 
    -------GUI--------------- COLOR PICKER --------------------
    -- update Values in Color Picker
    [bRIn,bGIn,bBIn] <- mapM (stepper "0") $ map UI.valueChange [elrVal,elgVal,elbVal]
    
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

    -- How can I make this smaller?

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
    
    on UI.click elBclearPal $ const $ do UI.clearCanvas canvas
                                        --val <- getPallette
                                        --liftIOLater $ print $ val
    -------GUI--------------- DITHER --------------------------    
    -- apply Dither and change to new imgDith
    on UI.click elBapplyDither $ return $ do [url] <- getValuesList [elURL]
                                             liftIO $ print url
                                             pal   <- getPallette
                                             liftIO $ testDither (whitePix:pal) url "./images/temp"
                                             delete elIimgOrig
                                             elIimgOrig  <- UI.image
                                                                # set UI.height 300
                                                                # set UI.width  300
                                                                # set style [("border", "solid black 1px")]
                                                                # set UI.src "static/temp_dith.png"
                                             element elDimgs #+ [element elIimgOrig]

    on UI.click elBsplice $ return $ do [url] <- getValuesList [elURL]
                                        liftIO $ print url
                                        pal   <- getPallette
                                        liftIO $ processImage (whitePix:pal) url "./images/temp"
                                        element elIimgOrig # set UI.src "static/temp_dith.png"











