{-|
Module      : Simulation.Types
Description : This module contains the definition of the type and the model of the Simulation System
Copyright   : (c) Juan Pablo Royo Sales, 2020
License     : GPL-3
Maintainer  : juanpablo.royo@gmail.com
Stability   : educational
Portability : POSIX

-}

module Simulation.Types where

import           Data.Default
import           GHC.Show
import           Protolude    as P
import qualified Data.PQueue.Min               as Q
import           Lens.Micro                    as L
import           Lens.Micro.TH


data Server = Server
  { _inQueue   :: Q.MinQueue Entity
  , _inProcess :: Maybe Entity
  }

instance Default Server where
  def = Server Q.empty Nothing

instance Show Server where
  show Server {..} =
    "In Queue: "
      <> (P.show $ Q.size _inQueue)
      <> " - In Process: "
      <> P.show _inProcess

data Entity = Entity
  { _arrival   :: Integer
  , _execTime  :: Integer
  , _serveTime :: Integer
  } deriving Show

data SimSystem = SimSystem
  { _currentTime    :: Integer
  , _servers        :: Server
  , _entitiesServed :: [Entity]
  , _stats          :: SimStats
  } deriving (Generic, Default, Show)


data SimStats = SimStats
  { _customerAmount   :: Integer
  , _waitingAmountSum :: Integer
  , _maxWaitingTime   :: Integer
  , _countWaiting     :: Integer
  , _queueLengthSum   :: Integer
  , _maxQueueLength   :: Integer
  , _countQueueNEmpty :: Integer
  } deriving (Generic, Default, Show)

averageWaitingTime :: SimSystem -> Integer
averageWaitingTime SimSystem {..}
  | _countWaiting _stats > 0 = _waitingAmountSum _stats `div` _countWaiting _stats
  | otherwise = 0

maxWaitTime :: SimSystem -> Integer
maxWaitTime = _maxWaitingTime . _stats

diffWaitingTime :: SimSystem -> Integer
diffWaitingTime s = abs ((averageWaitingTime s) - (_maxWaitingTime $ _stats s))

averageQueueSize :: SimSystem -> Integer
averageQueueSize SimSystem {..}
  | _countQueueNEmpty _stats > 0 = _queueLengthSum _stats `div` _countQueueNEmpty _stats
  | otherwise = 0


maxQueueSize :: SimSystem -> Integer
maxQueueSize = _maxQueueLength . _stats

diffQueueSize :: SimSystem -> Integer
diffQueueSize s = abs ((averageQueueSize s) - (_maxQueueLength $ _stats s))

makeLenses ''Entity
makeLenses ''SimStats
makeLenses ''SimSystem
makeLenses ''Server

instance Eq Entity where
  entA == entB = entA ^. arrival == entB ^. arrival

instance Ord Entity where
  entA <= entB = entA ^. arrival <= entB ^. arrival


