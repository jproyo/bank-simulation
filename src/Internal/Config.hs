module Internal.Config where

import           Data.Statistics.Distributions
import           GHC.Show
import           Lens.Micro.TH
import           Protolude                     as P

data SimConfig = SimConfig
  { _maxRunningTime   :: Integer
  , _betaDistribution :: Beta
  , _expDistribution  :: Exponential
  }

instance Show SimConfig where
  show SimConfig{..} =
    "\n ----- SIMULATION CONFIG ------ " <>
    "\nMax Running Time in Sec: " <> (P.show _maxRunningTime) <>
    "\nBeta Distribution: \\alpha[" <> (P.show $ _alphaB _betaDistribution) <> "] - \\beta[" <> (P.show $ _beta _betaDistribution) <> "]" <>
    "\nExponential Distribution: \\alpha[" <> (P.show $ _alphaE _expDistribution) <> "]" <>
    "\n ------------------------------ "

makeLenses ''SimConfig


