module Main where

import           Options.Applicative           as Opts
import           Protolude
import           Simulation


main :: IO ()
main =
  Opts.execParser configParser
    >>= readFileConf
    >>= executeSimulation
    >>= print
    .   _stats
