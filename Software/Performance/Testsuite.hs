import Core.MakeIMG
import Core.Dither
import Core.Colorsplicer
import Codec.Picture.Types
import Core.ProcessImage
import Data.List
import System.Random


main =  do 
    processImage "0" [PixelRGB8 255 150 0,greenPix,magentaPix] "./images/t9.png" "./images/test"
    

{- 		
        Ohne DS  Seq: 153913mm 	 0.24s
400x400 Mit  DS  Seq:  70158mm    0.46s
			 DSP Seq:  69955mm    1.24s

400x400 distSort    :  64657mm     0.74s
			 foo    :  64657mm     0.78s
600x600 distSort    :  143811mm    9.07s
		   	 foo    :  143811mm    9.25s
600x600 distSort    :  143811mm    8.35s
		     foo    :  145163mm    2.49s =^ 99,06% / 0.29%

1000x1000 distSort   : 389816.25 142.94s
			   foo   : 393617.5   33.25s =^ 99,06% / 0.23%

foo vs fooPar
1000x1000     foo     393617.5 32.64s
			  fooPar  xxx

foo vs fooPar vs distSort
500x500        mm           time
foo            101026.48   1.23s
fooPar[20]     289257.28   0.08s
distSort       100134.82   2.84s
fooPar[100]	   163754.72   0.16s
fooPar[200]	   129470.12   0.57s
fooPar[500]	   110460.79   2.88s
fooPar[100]	   163754.72   0.16s
distSort[1500] 103152.83   0.66s

processImage "0" [PixelRGB8 255 150 0,greenPix,magentaPix] "./images/t2.png" "./images/test"
255x255  		time     sparks 
par/sort 		0.91s    SPARKS: 6 (4 converted, 0 overflowed, 0 dud, 1 GC'd, 1 fizzled)
part/sortpar 	2.77s	 SPARKS: 30 (14 converted, 0 overflowed, 0 dud, 1 GC'd, 15 fizzled)	
seq/sor 		0.61s    SPARKS: 3 (2 converted, 0 overflowed, 0 dud, 1 GC'd, 0 fizzled)


-}
