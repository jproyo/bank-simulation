module Main where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Yaml.Config
import           GHC.Show
import           Options.Applicative as Opt
import           Options.Generic
import           Protolude           as P
import           Simulation

data Customer = Yellow
              | Blue
              | Red
              deriving (Generic, Read, Show)

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
  , showAll      :: Bool
  } deriving (Generic, Read)

instance ParseField ConfigSelection
instance ParseRecord ConfigSelection

instance Show ConfigSelection where
  show ConfigSelection{..} =
    "\n------- BANK CONFIG SELECTION ------ " <>
    "\nCustomer Type: " <> P.show custType <>
    "\nShow Average Wait Time: " <> P.show showAvgWait <>
    "\nShow Average Queue Length: " <> P.show showAvgQueue <>
    "\nShow Maximum Wait Time: " <> P.show showMaxWait <>
    "\nShow Maximum Queue Length: " <> P.show showMaxQueue <>
    "\nShow Diff between Average and Maximum: " <> P.show showDiff <>
    "\n------------------------------------ "

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

allConfigs :: [(ConfigSelection -> Bool, Text, SimSystem -> Integer)]
allConfigs = [(showAvgWait, "\nAverage Waiting Time: ", averageWaitingTime)
            , (showAvgQueue, "\nAverage Queue Size: ", averageQueueSize)
            , (showMaxWait, "\nMax Waiting Time: ",  maxWaitTime)
            , (showMaxQueue, "\nMax Queue Size: ",  maxQueueSize)
            , (showDiff, "\nDiff Waiting Time: ", diffWaitingTime)]

showResults :: ConfigSelection -> SimSystem -> IO ()
showResults conf sis = do
  configs <- filterM (\(b, _, _) -> pure $ showAll conf || b conf) allConfigs
  putText "\n--------- BANK SIMULATION RESULT ----------"
  putText $ foldMap (\(_, t, f) -> t <> (P.show $ f sis)) configs
  putText "\n-------------------------------------------"

main :: IO ()
main = do
  putText "\n----- BANK SIMULATION PROGRAM ------\n"
  config <- getRecord "Bank Simulation Program"
  putText $ P.show config
  confSim <- readFileConf (fileConfig config) >>= pure . mapToLibConf config
  putText $ P.show confSim
  result <- executeSimulation confSim
  showResults config result
