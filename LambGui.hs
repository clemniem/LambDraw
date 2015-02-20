import Control.Monad
import Safe 
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Dither
import LoadImage
{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
numCols = 4

safeColor :: Maybe Int -> Maybe Int -> Maybe Int -> UI.Color 
safeColor mr mg mb = UI.RGB (tst mr) (tst mg) (tst mb)
    where tst Nothing  = 0
          tst (Just i)
            | i > 255   = 255
            | i < 0     = 0
            | otherwise = i 

main :: IO ()
main = startGUI defaultConfig { 
            tpStatic     = Just "./images"
            } setup

setup :: Window -> UI ()
setup window = do
    return window # set title "LambDraw"


    canvas <- UI.canvas
        # set UI.height 35
        # set UI.width  35
        # set style [("border", "solid black 1px")]
    palCanvas <- UI.canvas
        # set UI.height 35
        # set UI.width  (35*4)
        # set style [("border", "solid black 1px")]
    imgur  <- UI.image
        # set UI.height 300
        # set UI.width  300
        # set style [("border", "solid black 1px")]
        # set UI.src "static/canvas.png"
    imgur2  <- UI.image
        # set UI.height 300
        # set UI.width  300
        # set style [("border", "solid black 1px")]
        # set UI.src "static/t2.png"
    drawWidth   <- UI.input -- dw
    drawHeight  <- UI.input -- dh
    applyResize <- UI.button #+ [string "Apply new Size."]
    
    elrVal <- UI.input
    elgVal <- UI.input
    elbVal <- UI.input

    addCol1    <- UI.button #+ [string "A"]
    addCol2    <- UI.button #+ [string "B"]
    addCol3    <- UI.button #+ [string "C"]
    addCol4    <- UI.button #+ [string "D"]

    removeColor <- UI.button #+ [string "Remove Color from Palette"]

    -- elCol1chek <-  UI.input # set UI.type_ "checkbox"
    -- elCol2chek <-  UI.input # set UI.type_ "checkbox"
    -- elCol3chek <-  UI.input # set UI.type_ "checkbox"
    -- elCol4chek <-  UI.input # set UI.type_ "checkbox"

    colPick <- UI.div #+ [row [column [grid [[string "R",element elrVal], [string "G", element elgVal],[string "B", element elbVal]]
                         ,row [element canvas, column [row [element addCol1,element addCol2,element addCol3,element addCol4]
                                                      ,element removeColor]]]
                         ,column [element palCanvas]]
                         ]
        # set UI.height 300
        # set UI.width  300
        # set style [("left", "50 px")]
        # set UI.align "top"
        # set UI.valign "left"




    addRects <- UI.button #+ [string "Add some rectangles."]
    addArcs  <- UI.button #+ [string "Add some arcs and circles."]
    clear    <- UI.button #+ [string "Clear the canvas."]

    getBody window #+ [
        element colPick, row [element imgur, element imgur2]
        ]

        -- row [column [ element imgur,
        --        row  [ grid [[string "Red", element elrVal]
        --               ,[string "Green",element elgVal]
        --               ,[string "Blue",element elbVal]]]
        --             ,element canvas]] 
        -- ,grid [[string "DrawWidth :" , element drawWidth  ]
        --       ,[string "drawHeight:" , element drawHeight ]
        --       ,[element applyResize]]
        -- , element addRects, element addArcs, element clear

    -- How can I make this smaller?
    bRIn <- stepper "0" $ UI.valueChange elrVal
    bGIn <- stepper "0" $ UI.valueChange elgVal
    bBIn <- stepper "0" $ UI.valueChange elbVal

    let updateCol = const $ do 
        rIn  <- currentValue bRIn
        gIn  <- currentValue bGIn
        bIn  <- currentValue bBIn
        element canvas # set UI.fillStyle (UI.solidColor $ safeColor (readMay rIn :: Maybe Int) 
                                                                     (readMay gIn :: Maybe Int) 
                                                                     (readMay bIn :: Maybe Int))
        UI.fillRect (0,0) 35 35 canvas
    
    -- How can I make this smaller?
    on UI.valueChange elrVal updateCol
    on UI.valueChange elgVal updateCol
    on UI.valueChange elbVal updateCol

    let updatePal nr = const $ do
        rIn  <- currentValue bRIn
        gIn  <- currentValue bGIn
        bIn  <- currentValue bBIn
        element palCanvas # set UI.fillStyle (UI.solidColor $ safeColor (readMay rIn :: Maybe Int) 
                                                                     (readMay gIn :: Maybe Int) 
                                                                     (readMay bIn :: Maybe Int))
        UI.fillRect ((nr*35),0) 35 35 palCanvas
    
    -- How can I make this smaller?
    on UI.click addCol1 $ updatePal 0
    on UI.click addCol2 $ updatePal 1
    on UI.click addCol3 $ updatePal 2
    on UI.click addCol4 $ updatePal 3

    on UI.click removeColor $ const $ UI.clearCanvas canvas

    dwIn   <- stepper "0" $ UI.valueChange drawWidth
    on UI.click applyResize $ const $ element drawHeight # sink value dwIn
    
    let procImg =
          return $ liftIOLater $ processImage 1 "./images/canvas.png" "./images/tempDith"
          

    on UI.click applyResize procImg
    on UI.click applyResize $ return $ element imgur # set UI.src "static/tempDith.png"


    let rects = [ (x , 0, 35, 35, 0) | x <- [0..3]]

                
    let drawRect (x,y,w,h,col) = do
          element canvas # set UI.fillStyle (UI.solidColor $ UI.RGB 255 255 0)
          UI.fillRect (x,y) w h canvas

    on UI.mousedown addRects $ const $ forM_ rects drawRect

    let circles = [ (200, 200, 25, "orange")
                  , (300, 180, 15, "plum")
                  , (100, 180, 15, "plum")
                  ]
    let drawCircle (x,y,r,col) = do
          element canvas # set UI.fillStyle (UI.htmlColor col)
          UI.beginPath canvas
          UI.arc (x,y) r 0 (2*pi) canvas
          UI.fill canvas

    let slices = [ (325, 115, 25, 1, 2, "lightblue")
                 , (325, 145, 25, 1, 2, "lightblue")
                 ]
    let drawSlice (x,y,r,start,end,col) = do
          element canvas # set UI.fillStyle (UI.htmlColor col)
          UI.beginPath canvas
          UI.arc (x,y) r start end canvas
          UI.lineTo (x,y) canvas
          UI.stroke canvas
          UI.closePath canvas
          UI.fill canvas

    on UI.click addArcs $ const $ do
      forM_ circles drawCircle
      forM_ slices  drawSlice
      element canvas # set UI.textFont "42pt sans-serif"
      UI.fillText "Canvas" (100,100) canvas
      UI.strokeText "Canvas" (100,100) canvas

    on UI.click clear  $ const $ UI.clearCanvas canvas