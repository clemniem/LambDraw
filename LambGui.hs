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
        # set UI.height 400
        # set UI.width  400
        # set style [("border", "solid black 1px")]
    imgur  <- UI.image
        # set UI.height 400
        # set UI.width  400
        # set style [("border", "solid black 1px")]
        # set UI.src "static/canvas.png"
    drawWidth   <- UI.input -- dw
    drawHeight  <- UI.input -- dh
    applyResize <- UI.button #+ [string "Apply new Size."]
    
    addRects <- UI.button #+ [string "Add some rectangles."]
    addArcs  <- UI.button #+ [string "Add some arcs and circles."]
    clear    <- UI.button #+ [string "Clear the canvas."]

    getBody window #+ [
        row [element imgur, element canvas]
        ,grid [[string "DrawWidth :" , element drawWidth  ]
              ,[string "drawHeight:" , element drawHeight ]
              ,[element applyResize]]
        , element addRects, element addArcs, element clear
        ]

    dwIn   <- stepper "0" $ UI.valueChange drawWidth
    on UI.click applyResize $ const $ element drawHeight # sink value dwIn
    
    let procImg =
          return $ liftIOLater $ processImage 1 "./images/canvas.png" "./images/tempDith"
          

    onEvent (UI.click applyResize) procImg
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