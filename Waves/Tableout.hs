module Tableout where

import Waves.Types
import Waves.Math

import Text.Printf

-- String suitable for gnuplot (use writeFile -- may take megabytes!)
--                     Step width
print2D :: WaveField -> Double -> Time -> String
print2D w@(WaveField (Medium c xdim ydim) exciters) step t = [0,0+step..xdim] >>= \x -> [0,0+step..ydim] >>= \y -> show x ++ " " ++ show y ++ " " ++ printf "%.15f" (wave2d w (x,y) t) ++ "\n"
