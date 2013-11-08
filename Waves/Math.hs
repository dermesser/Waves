module Waves.Math where

import Data.List

import Waves.Types

-- Pythagoras, basically. To be easily extended for 3d waves
calcDistance :: Point -> Point -> Distance
calcDistance (x1,y1) (x2,y2) = sqrt ((x1-x2)**2 + (y1-y2)**2)

-- 1D wave equation. Gives amplitude at location, time with initial phase delay phi0
waveAt1d :: Exciter -> Distance -> Time -> Amplitude
waveAt1d (Exciter wlen freq phi0 maxamp _) x t = maxamp * sin ( (2*pi* ( (t*freq) - (x/wlen) ) ) + phi0 )


-- 1D interferences

wave1d :: [Exciter] -> Distance -> Time -> Amplitude
wave1d exciters s t = foldr1 (+) . map getAmp $ exciters
    where getAmp e = if s <= ((getFrequency e) * (getLambda e))  * t
                     then waveAt1d e s t
                     else 0

-- 2D interferences
wave2d :: WaveField -> Point -> Time -> Amplitude
wave2d (WaveField _medium exciters) p t = foldr1 (+) . map getAmp $ exciters
    where distance e = calcDistance (getPoint e) p
          getAmp e = let d = distance e in
                     if d <= ((getFrequency e) * (getLambda e)) * t
                     then waveAt1d e (distance e) t
                     else 0
