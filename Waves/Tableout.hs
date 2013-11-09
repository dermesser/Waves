module Tableout where

import Waves.Types
import Waves.Math

import Data.List
import Text.Printf

-- String suitable for gnuplot (use writeFile -- may take megabytes!)
--                     Step width
print2D :: WaveField -> Double -> Time -> String
print2D w@(WaveField (Medium c xdim ydim) exciters) step t = concatMap xfunc xlist
    where xlist = [0,0+step..xdim]
          ylist = [0,0+step..ydim]
          xfunc x = (concatMap (\y -> yfunc x y) ylist) ++ "\n"
          yfunc x y = show x ++ " " ++ show y ++ " " ++ printf "%.3f" (wave2d w (x,y) t) ++ "\n"
