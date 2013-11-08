module Waves.Types where

type Amplitude = Double
type Distance = Double
type Wavelength = Double
type Frequency = Double
type Phase = Double
type Velocity = Double

type Time = Double
type Point = (Double, Double)

data Medium = Medium { getC :: Velocity -- c (wave velocity)
                     , getXdim :: Double -- Width of medium
                     , getYdim :: Double -- Height of Medium
} deriving Show

data Exciter = Exciter { getLambda :: Wavelength
                       , getFrequency :: Frequency
                       , getPhi :: Phase -- Phi (initial phase delay)
                       , getAmplitude :: Amplitude
                       , getPoint :: Point
} deriving Show

data WaveField = WaveField { getMedium :: Medium
                           , getExciters :: [Exciter]
} deriving Show

{-
data PointTime = PointTime { getPosition :: Point
                           , getTime :: Time
} deriving Show
-}

-- extra constructors

-- exciters
exciterWL :: Medium -> Distance -> (Phase -> Amplitude -> Point -> Exciter)
exciterWL (Medium c _ _) wl = Exciter wl (c/wl)

exciterFreq :: Medium -> Frequency -> (Phase -> Amplitude -> Point -> Exciter)
exciterFreq (Medium c _ _) f = Exciter (c/f) f

-- points
midPoint :: Medium -> Point
midPoint (Medium _ x y) = (x/2,y/2)
