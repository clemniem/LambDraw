import Control.Monad

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Dither
import LoadImage
{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
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
    
    rVal <- UI.input
    gVal <- UI.input
    bVal <- UI.input

    addColor    <- UI.button #+ [string "Add Color to Palette     "]
    removeColor <- UI.button #+ [string "Remove Color from Palette"]

    colPick <- UI.div #+ [grid [[string "R",element rVal], [string "G", element gVal],[string "B", element bVal]]
                         ,row [element canvas, column [element addColor, element removeColor]]
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
        --        row  [ grid [[string "Red", element rVal]
        --               ,[string "Green",element gVal]
        --               ,[string "Blue",element bVal]]]
        --             ,element canvas]] 
        -- ,grid [[string "DrawWidth :" , element drawWidth  ]
        --       ,[string "drawHeight:" , element drawHeight ]
        --       ,[element applyResize]]
        -- , element addRects, element addArcs, element clear

    bRIn <- stepper "0" $ UI.valueChange rVal
    bGIn <- stepper "0" $ UI.valueChange gVal
    bBIn <- stepper "0" $ UI.valueChange bVal

    on UI.click addColor $ const $ do
        rIn  <- currentValue bRIn
        gIn  <- currentValue bGIn
        bIn  <- currentValue bBIn
        element canvas # set UI.fillStyle (UI.solidColor $ UI.RGB (read rIn :: Int) 
                                                                  (read gIn :: Int) 
                                                                  (read bIn :: Int))
        UI.fillRect (0,0) 35 35 canvas
        --liftIOLater $ putStrLn rIn

    on UI.click removeColor $ const $ UI.clearCanvas canvas

    dwIn   <- stepper "0" $ UI.valueChange drawWidth
    on UI.click applyResize $ const $ element drawHeight # sink value dwIn
    
    let procImg =
          return $ liftIOLater $ processImage 1 "./images/canvas.png" "./images/tempDith"
          

    on UI.click applyResize procImg
    on UI.click applyResize $ return $ element imgur # set UI.src "static/tempDith.png"


    let rects = [ (x , y, 10, 10, "red") |  y <-[0,10..200], x <- [0,20..300] 

                ]
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