module Internal.Config where

import           Data.Statistics.Distributions
import           Lens.Micro.TH
import           Protolude

data SimConfig = SimConfig
  { _maxRunningTime   :: Integer
  , _betaDistribution :: Beta
  , _expDistribution  :: Exponential
  } deriving Show

makeLenses ''SimConfig


