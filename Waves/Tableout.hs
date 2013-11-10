module Tableout where

import Waves.Types
import Waves.Math

import Text.Printf

-- String suitable for gnuplot 3D plotting (use writeFile -- may take megabytes!)
--                     Step width
print2D :: WaveField -> Double -> Time -> String
print2D w@(WaveField (Medium c xdim ydim) exciters) step t = concatMap xfunc xlist
    where xlist = [0,0+step..xdim]
          ylist = [0,0+step..ydim]
          xfunc x = (concatMap (\y -> yfunc x y) ylist) ++ "\n"
          yfunc x y = printf "%.3f" x ++ " " ++ printf "%.3f" y ++ " " ++ printf "%.3f" (wave2d w (x,y) t) ++ "\n"

-- Only print wave fronts
-- Gives (x,y) coordinates (points in R²) where the waves are higher than (maximum amplitude - threshold; if threshold is > 0) or lower than (minimum + threshold; if threshold < 0)
printFlat :: WaveField -> Double -> Time -> Double -> String
printFlat w@(WaveField (Medium c xdim ydim) exciters) step t threshold = concatMap xfunc xlist
    where xlist = [0,0+step..xdim]
          ylist = [0,0+step..ydim]
          maxamp = (if threshold < 0 then (-1) else 1) * foldl (\a e -> a + (getAmplitude e)) 0 exciters
          ampCompare = if threshold < 0 then (<) else (>)
          xfunc x = concatMap (\y -> yfunc x y) ylist
          yfunc x y = let amplitude = wave2d w (x,y) t in
                          if amplitude `ampCompare` (maxamp - threshold)
                          then print2Dcoord x y
                          else ""
          print2Dcoord x y = printf "%.3f" x ++ " " ++ printf "%.3f" y ++ "\n"
