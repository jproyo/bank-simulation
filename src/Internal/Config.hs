module Internal.Config where

import           Data.Aeson
import           Data.Statistics.Distributions
import           Data.Yaml.Config
import           Lens.Micro.TH
import           Options.Applicative           as Opt
import           Protolude

data SimConfig = SimConfig
  { _maxRunningTime   :: Integer
  , _betaDistribution :: Beta
  , _expDistribution  :: Exponential
  } deriving Show

makeLenses ''SimConfig

instance FromJSON SimConfig where
  parseJSON = withObject "simConfig" $ \obj -> do
    _maxRunningTime <- obj .: "max_running_time_in_sec"
    _betaDistribution <- obj .: "beta_distribution"
    _expDistribution <- obj .: "exp_distribution"
    return SimConfig{..}

configurationFile :: Opt.Parser FilePath
configurationFile = Opt.strOption
  (  Opt.long "configuration"
  <> Opt.short 'f'
  <> Opt.metavar "FilePath"
  <> Opt.help "Configuration file with simulation parameters"
  )

configParser :: Opt.ParserInfo FilePath
configParser = Opt.info
  (configurationFile <**> Opt.helper)
  (  Opt.fullDesc
  <> Opt.progDesc "Simulation System Program based on Event Scheduling"
  <> Opt.header "Event Scheduling Simulation System"
  )

readFileConf :: FilePath -> IO SimConfig
readFileConf file = loadYamlSettings [file] [] useEnv

