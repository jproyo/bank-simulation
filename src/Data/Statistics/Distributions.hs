{-|
Module      : Data.Statistics.Distributions
Description : This module contains config definition for the statistics distributions to generate events arrivals and service times
Copyright   : (c) Juan Pablo Royo Sales, 2020
License     : GPL-3
Maintainer  : juanpablo.royo@gmail.com
Stability   : educational
Portability : POSIX
-}
module Data.Statistics.Distributions where

import           Data.Aeson
import           Protolude
import           System.Random

class Num a => TimeSeriesGen t a | t -> a where
  genArrival :: RandomGen g => t -> g -> (a, g)

class Num a => TimeServiceGen t a | t -> a where
  genServiceTime :: RandomGen g => t -> g -> (a, g)

data Beta = Beta
  { _alphaB :: Integer
  , _beta   :: Integer
  } deriving Show

instance FromJSON Beta where
  parseJSON =
    withObject "beta" $ \obj -> Beta <$> obj .: "alpha" <*> obj .: "beta"

newtype Exponential = Exponential { _alphaE :: Integer }
  deriving Show

instance FromJSON Exponential where
  parseJSON = withObject "beta" $ \obj -> Exponential <$> obj .: "alpha"

instance TimeSeriesGen Exponential Integer where
  genArrival Exponential {..} g =
    let (r, g') = randomR @Float (0.0, 1.0) g
    in  (round ((-(fromInteger _alphaE)) * log (1.0 - r)), g')

instance TimeServiceGen Beta Integer where
  genServiceTime Beta {..} g =
    let (v, g') = randomR @Float (0.0, 1.0) g
    in  ( round
          ( 200
          * (v ^ ((_alphaB - 1) :: Integer))
          * ((1 - v) ^ ((_beta - 1) :: Integer))
          )
        , g'
        )


