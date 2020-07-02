module Main where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Yaml.Config
import           Options.Applicative as Opt
import           Options.Generic
import           Protolude
import           Simulation

data Customer = Yellow
              | Blue
              | Red
              deriving (Generic, Read)

instance ParseField Customer
instance ParseFields Customer
instance ParseRecord Customer

data CustomerConfig = CustomerConfig
  { customerType :: Customer
  , alphaVal     :: Integer
  , betaVal      :: Integer
  }

data ConfigSim = Config
  { yellow       :: CustomerConfig
  , blue         :: CustomerConfig
  , red          :: CustomerConfig
  , maxRunTime   :: Integer
  , alphaExpDist :: Integer
  }

data ConfigSelection = ConfigSelection
  { fileConfig   :: FilePath
  , custType     :: Customer
  , showAvgWait  :: Bool
  , showAvgQueue :: Bool
  , showMaxWait  :: Bool
  , showMaxQueue :: Bool
  , showDiff     :: Bool
  } deriving (Generic, Read)

instance ParseField ConfigSelection
instance ParseRecord ConfigSelection

toCustomerConfig :: Customer -> Object -> Data.Aeson.Types.Parser CustomerConfig
toCustomerConfig cust =
  \obj -> CustomerConfig cust <$> obj .: "alpha" <*> obj .: "beta"

instance FromJSON ConfigSim where
  parseJSON = withObject "simConfig" $ \obj -> do
    maxRunTime   <- obj .: "max_running_time_in_sec"
    yellow       <- obj .: "yellow" >>= toCustomerConfig Yellow
    red          <- obj .: "red" >>= toCustomerConfig Red
    blue         <- obj .: "blue" >>= toCustomerConfig Blue
    alphaExpDist <- obj .: "exp_distribution" >>= (.: "alpha")
    return Config { .. }

readFileConf :: FilePath -> IO ConfigSim
readFileConf file = loadYamlSettings [file] [] useEnv

fromCustTypeToBeta :: CustomerConfig -> Beta
fromCustTypeToBeta CustomerConfig{..} = Beta alphaVal betaVal

mapToLibConf :: ConfigSelection -> ConfigSim -> SimConfig
mapToLibConf ConfigSelection{..} Config {..} =
  let betaDist = case custType of
                      Yellow -> fromCustTypeToBeta yellow
                      Red    -> fromCustTypeToBeta red
                      Blue   -> fromCustTypeToBeta blue
   in SimConfig maxRunTime betaDist (Exponential alphaExpDist)

main :: IO ()
main = do
  putText "----- BANK SIMULATION PROGRAM ------"
  config <- getRecord "Bank Simulation Program"
  readFileConf (fileConfig config)
    >>= pure .  mapToLibConf config
    >>= executeSimulation
    >>= print . averageWaitingTime
