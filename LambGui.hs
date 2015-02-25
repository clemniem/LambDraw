import Control.Monad
import Safe 

import Dither
import LoadImage
import MakeIMG as I
import Colorsplicer
import Data.Ratio
import Resize

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Codec.Picture.Types

ifSize :: Maybe (Int,Int) -> (Int,Int)
ifSize (Just msize) = msize
ifSize _           = (0,0)

-- DIN A4 210 x 297 mm
drawMax = (210,297) -- (width,height)


padding = [("padding","10px 10px 10px 10px")]
-- ImgMax -> DrawMax -> Bool
imgTooBig :: (Int,Int) -> (Int,Int) -> Bool
imgTooBig (imW,imH) (dW,dH) = and [imW>dW,imH>dH]

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

toPallette :: (PixelRGB8,PixelRGB8,PixelRGB8,PixelRGB8) -> [PixelRGB8]
toPallette (a,b,c,d) = remWhite [a,b,c,d]
  where remWhite :: [PixelRGB8] -> [PixelRGB8]
        remWhite [] = []
        remWhite (x:xs)
          | checkColor x whitePix = remWhite xs
          | otherwise             = x : remWhite xs

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
    toInt _         = 0  

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}

main :: IO ()
main = startGUI defaultConfig { 
            tpStatic     = Just "./images"
            } setup

{-----------------------------------------------------------------------------
    Setup
------------------------------------------------------------------------------}

setup :: Window -> UI ()
setup window = do
    return window # set title "LambDraw"
    ---------------------- SETUP --------------------------
    elDivHIDE <- UI.div -- used to Hide elements
    elURL     <- UI.div 
    ---------------------- LOADIMG -------------------------
    elIpathIn <- UI.input #+ [string "PathIN"]
    elBload   <- UI.button #+ [string "Load File."]


    elDload   <- UI.div #+ [row [element elIpathIn,element elBload]] 
    elDimgs   <- UI.div
    elIimgOrig  <- UI.image
        # set UI.height 300
        # set UI.width  300
        # set style [("border", "solid black 1px")]
        # set UI.src "static/canvas.png"

    ---------------------- RESIZE --------------------------
    elIimgWidth    <- UI.span # set UI.text "---" -- img w
                              # set style padding
    elIimgHeight   <- UI.span # set UI.text "---"-- img h    
                              # set style padding
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
    elDresizeInv   <- UI.div #+ [string "Image too big, needs Resizing:",
                               row [element elBlsresize] # set style padding, 
                                grid [[string "img Width: ", element elIimgWidth, 
                                        string "dWidth:", element elIdrawWidth  ],
                                      [string "img Height: ",element elIimgHeight, 
                                        string "dHeight:", element elIdrawHeight]]]
                           # set style padding
    ---------------------- COLOR PICKER --------------------
    elrVal <- UI.input
    elgVal <- UI.input
    elbVal <- UI.input

    addCol1    <- UI.button #+ [string "A"]
    addCol2    <- UI.button #+ [string "B"]
    addCol3    <- UI.button #+ [string "C"]
    addCol4    <- UI.button #+ [string "D"]

    elBgetPall <- UI.button #+ [string "Get Pallette*DEBUG*"]
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
                         ,row [element canvas, column [row [element addCol1,element addCol2,element addCol3,element addCol4]
                                                      ,element removeColor]]]
                         ,column [element palCanvas, element elBgetPall]]
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
    bUrlIn <- stepper "" $ UI.valueChange elIpathIn
    
    let readSize = do urlIn   <- currentValue bUrlIn
                      -- ??? Wie kann ich einfacher die IO wegbekommen. bzw IO -> UI
                      ioMsize <- liftIO $ return $ getImgSize urlIn
                      mSize   <- liftIO ioMsize
                      let sze    = ifSize mSize -- (width,height)
                      let width  = show $ fst sze
                      let height = show $ snd sze
                      element elIimgHeight # set UI.text  height
                                           # set UI.value height
                      element elIimgWidth  # set UI.text  width
                                           # set UI.value width
                      if sze > drawMax
                        then element elDresize #+ [element elDresizeInv]
                        else element elDresize #+ [string "No Resize necessary"]
                      liftIOLater $ print sze

    on UI.click elBload $ const $ do urlIn <- currentValue bUrlIn
                                     uri <- loadFile "image" urlIn
                                     element elURL # set UI.value urlIn
                                     element elIimgOrig # set UI.src uri
                                     element elDimgs #+ [element elIimgOrig]
                                     readSize  

    -------GUI--------------- RESIZE --------------------------

    rwIn   <- stepper "0" $ UI.valueChange elIdrawWidth
    rhIn   <- stepper "0" $ UI.valueChange elIdrawHeight

    --on UI.valueChange rwIn $ updateResizeInput
    -- 
    on UI.click elBcalcSize $ return $ do rwStr  <- currentValue rwIn
                                          rhStr  <- currentValue rhIn
                                          [imwStr] <- getValuesList [elIimgWidth]
                                          [imhStr] <- getValuesList [elIimgHeight]
                                          liftIOLater $ print imwStr
                                          let d@(dW,dH)   = drawMax 
                                          let i@(imW,imH) = safeTupel (imwStr,imhStr)
                                          let r@(rW,rH)   = safeTupel (rwStr,rhStr)
                                          let fact        = (show rW)++" "++show rH
                                          if (not $ and [d>r,r<i,r>(0,0)])
                                             then do element elIdrawWidth # set UI.value "invalid"
                                                     element elDivHIDE #+ [element elBapplyResize]
                                                     liftIOLater $ print "Wrong Input. Size is still to big"
                                             else do element elresFac # set UI.value fact
                                                     element elDresize #+ [element elBapplyResize]
                                                     liftIOLater $ print "Right input"
                                           


    on UI.click elBapplyResize $ return $ do urlIn <- currentValue bUrlIn
                                             [balub] <- getValuesList [elresFac]
                                             let [rh,rw] = words balub
                                             let (rh',rw')   = safeTupel (rh,rw)
                                             liftIO $ goResize urlIn "./images/tempRes.png" rh'
                                             element elIimgRes  # set UI.src "static/tempRes.png"
                                                                # set UI.height rh'
                                                                # set UI.width  rw'
                                             element elDimgs #+ [element elIimgRes]
                                             element elURL   # set UI.value "./images/tempRes.png"
                                             liftIOLater $ print rh' 
    -------GUI--------------- COLOR PICKER --------------------
    -- How can I make this smaller?
    bRIn <- stepper "0" $ UI.valueChange elrVal
    bGIn <- stepper "0" $ UI.valueChange elgVal
    bBIn <- stepper "0" $ UI.valueChange elbVal
    
    let updateCanv canv nr = const $ do
        rIn  <- currentValue bRIn
        gIn  <- currentValue bGIn
        bIn  <- currentValue bBIn
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

    on UI.click removeColor $ return $ UI.clearCanvas canvas
    
    let getPallette = mapM (getCanvCol palCanvas) [(1,1),(36,1),(72,1),(108,1)]
    
    on UI.click elBgetPall $ const $ do val <- getPallette
                                        liftIOLater $ print $ val
    -------GUI--------------- DITHER --------------------------    
    -- apply Dither and change to new imgDith
    on UI.click elBapplyDither $ return $ do [url] <- getValuesList [elURL]
                                             liftIO $ print url
                                             pal   <- getPallette
                                             liftIO $ processDither pal url "./images/tempDith"
                                             element elIimgOrig # set UI.src "static/tempDith.png"
                                             element elURL # set UI.value "./images/tempDith.png"
    -------GUI--------------- COLORSPLICER --------------------
    on UI.click elBsplice $ return $ do [url] <- getValuesList [elURL]
                                        [pixA,pixB,pixC,pixD] <- getPallette
                                        liftIO $ print pixA
                                        liftIO $ print pixB
                                        liftIO $ print pixC
                                        liftIO $ print pixD
                                        let getls p = if checkColor p whitePix
                                                        then return []
                                                        else do iols <- return $ liftIO $ processSplice url p
                                                                ls   <- iols
                                                                return ls
                                        lsA   <- getls pixA
                                        lsB   <- getls pixB 
                                        lsC   <- getls pixC
                                        lsD   <- getls pixD 
                                        liftIO $ print lsA
                                        liftIO $ print lsB 
                                        liftIO $ print lsC 
                                        liftIO $ print lsD 


    -------GUI--------------- GCODE ---------------------------
    -------GUI--------------- SAVEFILE ------------------------
    -------GUI--------------- BODY ----------------------------



    -- elCHbox <-  UI.input # set UI.type_ "checkbox"













    -- let rects = [ (x , 0, 35, 35, 0) | x <- [0..3]]

                
    -- let drawRect (x,y,w,h,col) = do
    --       element canvas # set UI.fillStyle (UI.solidColor $ UI.RGB 255 255 0)
    --       UI.fillRect (x,y) w h canvas

    -- on UI.mousedown addRects $ const $ forM_ rects drawRect

    -- let circles = [ (200, 200, 25, "orange")
    --               , (300, 180, 15, "plum")
    --               , (100, 180, 15, "plum")
    --               ]
    -- let drawCircle (x,y,r,col) = do
    --       element canvas # set UI.fillStyle (UI.htmlColor col)
    --       UI.beginPath canvas
    --       UI.arc (x,y) r 0 (2*pi) canvas
    --       UI.fill canvas

    -- let slices = [ (325, 115, 25, 1, 2, "lightblue")
    --              , (325, 145, 25, 1, 2, "lightblue")
    --              ]
    -- let drawSlice (x,y,r,start,end,col) = do
    --       element canvas # set UI.fillStyle (UI.htmlColor col)
    --       UI.beginPath canvas
    --       UI.arc (x,y) r start end canvas
    --       UI.lineTo (x,y) canvas
    --       UI.stroke canvas
    --       UI.closePath canvas
    --       UI.fill canvas

    -- on UI.click addArcs $ const $ do
    --   forM_ circles drawCircle
    --   forM_ slices  drawSlice
    --   element canvas # set UI.textFont "42pt sans-serif"
    --   UI.fillText "Canvas" (100,100) canvas
    --   UI.strokeText "Canvas" (100,100) canvas

    -- on UI.click clear  $ const $ UI.clearCanvas canvas























