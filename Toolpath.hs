import MakeIMG
import Dither
import Colorsplicer
import Codec.Picture.Types

main = print $ show (lsDistance starls)


-- 300x300 Mit  DS  Seq:  39978mm  	 6.09s
--         Mit  DS  Par:  39978mm	11.59s ???			
--         Ohne DS  Seq: 153913mm 	 0.24s
-- 400x400 Mit  DS  Seq:  70158mm    0.46s
--				DSP Seq:  69955mm    1.24s
--
--
--
--