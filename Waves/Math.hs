module Waves.Math where

import Data.List

import Waves.Types

-- Pythagoras, basically. To be easily extended for 3d waves
calcDistance :: Point -> Point -> Distance
calcDistance (x1,y1) (x2,y2) = sqrt ((x1-x2)**2 + (y1-y2)**2)

-- 1D wave equation. Gives amplitude at location, time with initial phase delay phi0
waveAt1d :: Exciter -> Distance -> Time -> Amplitude
waveAt1d (Exciter wlen freq phi0 maxamp _) x t = maxamp * sin ( (2*pi* ( (t*freq) - (x/wlen) ) ) + phi0 )


waveAt2d :: WaveField -> Point -> Time -> Amplitude
waveAt2d (WaveField _medium exciters) p t = foldr1 (+) . map getAmp $ exciters
    where distance e = calcDistance (getPoint e) p
          getAmp e = waveAt1d e (distance e) t
