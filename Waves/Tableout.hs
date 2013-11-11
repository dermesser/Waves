module Tableout where

import Waves.Types
import Waves.Math

import qualified Data.ByteString.Char8 as B
import Text.Printf

-- String suitable for gnuplot 3D plotting (use writeFile -- may take megabytes!)
--                     Step width
print2D :: WaveField -> Double -> Time -> B.ByteString
print2D w@(WaveField (Medium c xdim ydim) exciters) step t = B.concat . map xfunc $ xlist
    where xlist = [0,0+step..xdim]
          ylist = [0,0+step..ydim]
          xfunc x = (B.concat . map (\y -> yfunc x y) $ ylist) `B.append` (B.pack "\n")
          yfunc x y = B.pack $ printf "%.3f %.3f %.3f\n" x y (wave2d w (x,y) t)

-- Only print wave fronts
-- Gives (x,y) coordinates (points in R²) where the waves are higher than (maximum amplitude - threshold; if threshold is > 0) or lower than (minimum + threshold; if threshold < 0)
printFlat :: WaveField -> Double -> Time -> Double -> B.ByteString
printFlat w@(WaveField (Medium c xdim ydim) exciters) step t threshold = B.concat . map xfunc $ xlist
    where xlist = [0,0+step..xdim]
          ylist = [0,0+step..ydim]
          maxamp = (if threshold < 0 then (-1) else 1) * foldl (\a e -> a + (getAmplitude e)) 0 exciters
          ampCompare = if threshold < 0 then (<) else (>)
          xfunc x = foldr1 (B.append) $ map (\y -> yfunc x y) ylist
          yfunc x y = let amplitude = wave2d w (x,y) t in
                          if amplitude `ampCompare` (maxamp - threshold)
                          then print2Dcoord x y
                          else B.pack ""
          print2Dcoord x y = B.pack $ printf "%.3f %.3f\n" x y
