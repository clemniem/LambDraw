import Control.Monad
import Safe 

import Dither
import LoadImage
import MakeIMG
import Colorsplicer

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Codec.Picture.Types

ifSize :: Maybe (Int,Int) -> (Int,Int)
ifSize (Just msize) = msize
ifSize _           = (0,0)


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
    elIimgHeight   <- UI.span # set UI.text "---"-- img h    

    elIdrawWidth   <- UI.input -- dw
    elIdrawHeight  <- UI.input -- dh    

    elIimgRes      <- UI.image
        -- # set UI.height 300
        -- # set UI.width  300
        # set style [("border", "solid black 1px")]
        # set UI.src "static/t2.png"
    elBgetSize    <- UI.button #+ [string "getSize"]
    elUgetSize    <- UI.input

    elBapplyResize <- UI.button #+ [string "Apply Resize."]
    elDresize <- UI.div #+ [element elBgetSize, grid [[string "img Width: ", element elIimgWidth ],
                                                      [string "img Height: ",element elIimgHeight]]]
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
    elDdither <- UI.div #+ [element elBapplyDither, element elBapplyResize]    

    ---------------------- COLORSPLICER --------------------
    ---------------------- GCODE ---------------------------
    ---------------------- SAVEFILE ------------------------
    -- ======
--------------------------- BODY ----------------------------
    getBody window #+ [ 
        element elDload,
        element elDresize,
        row [element elDimgs],
        element elDdither, 
        element colPick
        ]

{-----------------------------------------------------------------------------
                        Start GUI
    ------------------------------------------------------------------------------}


    -------GUI--------------- SETUP ---------------------------

    -------GUI--------------- LOADIMG -------------------------
    bUrlIn <- stepper "" $ UI.valueChange elIpathIn
    on UI.click elBload $ const $ do urlIn <- currentValue bUrlIn
                                     uri <- loadFile "image" urlIn
                                     return elIimgOrig # set UI.src uri
                                     return elDimgs #+ [element elIimgOrig]  




    -------GUI--------------- RESIZE --------------------------
    on UI.click elBgetSize $ const $ do urlIn <- currentValue bUrlIn
                                        ioMsize <- liftIO $ return $ getImgSize urlIn
                                        mSize <- liftIO ioMsize
                                        let sze    = ifSize mSize -- (width,height)
                                        let width  = show $ fst sze
                                        let height = show $ snd sze
                                        element elIimgHeight # set UI.text height
                                        element elIimgWidth # set UI.text width
                                        liftIOLater $ print sze
    
    dwIn   <- stepper "0" $ UI.valueChange elIdrawWidth
    on UI.click elBapplyResize $ const $ element elIdrawHeight # sink value dwIn
    


    on UI.click elBapplyResize $ return $ element elDivHIDE #+ [element elIimgRes]
    -------GUI--------------- COLOR PICKER --------------------
    -- How can I make this smaller?
    bRIn <- stepper "0" $ UI.valueChange elrVal
    bGIn <- stepper "0" $ UI.valueChange elgVal
    bBIn <- stepper "0" $ UI.valueChange elbVal
    
    let updateCanv canv nr = const $ do
        rIn  <- currentValue bRIn
        gIn  <- currentValue bGIn
        bIn  <- currentValue bBIn
        let (r',g',b') = (readMay rIn,readMay gIn,readMay bIn) :: (Maybe Int,Maybe Int,Maybe Int)
        element canv # set UI.fillStyle (UI.solidColor $ safeColorUI r' g' b')
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
    
    let getPallette = const $ do  val <- mapM (getCanvCol palCanvas) [(1,1),(36,1),(72,1),(108,1)] 
                                  liftIOLater $ print $ val
    on UI.click elBgetPall getPallette
    -------GUI--------------- DITHER --------------------------    
    -- apply Dither and change to new imgDith
    on UI.click elBapplyDither $ return $ do pal <- mapM (getCanvCol palCanvas) [(1,1),(36,1),(72,1),(108,1)]
                                             liftIOLater $ processImagels pal "./images/canvas.png" "./images/tempDith"
    on UI.click elBapplyDither $ return $    element elIimgOrig # set UI.src "static/tempDith.png"
    -------GUI--------------- COLORSPLICER --------------------
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























