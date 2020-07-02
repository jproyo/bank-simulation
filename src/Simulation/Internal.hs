{-# LANGUAGE AllowAmbiguousTypes #-}
{-|
Module      : Simulation.Internal
Description : This module contains the definitions and combinators of core Simulation Library. The Simulation Library is a generic Event Scheduling model where we have a System, Servers in the System, Queues, Arrivals, etc.
Copyright   : (c) Juan Pablo Royo Sales, 2020
License     : GPL-3
Maintainer  : juanpablo.royo@gmail.com
Stability   : educational
Portability : POSIX

-}
module Simulation.Internal
  ( simulation
  , EventGenerator(..)
  , executeSimulation
  , T.SimSystem
  , T.averageWaitingTime
  , T.averageQueueSize
  , T.maxWaitTime
  , T.maxQueueSize
  , T.diffWaitingTime
  , T.diffQueueSize
  )
where

import           Data.Default
import qualified Data.PQueue.Min               as Q
import           Data.Statistics.Distributions
import           Lens.Micro                    as L
import           Protolude                     as P
import           Simulation.Config
import           Simulation.Types              as T
import           System.Random


class Monad m => EventGenerator m where
  generateEvents :: RandomGen g => g -> m [Entity]

type WithEventGenerator e b g
  = (TimeSeriesGen e Integer, TimeServiceGen b Integer, RandomGen g)

newtype Simulation a = Simulation { runSimulation :: StateT SimSystem (ReaderT SimConfig IO) a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadState SimSystem
                   , MonadReader SimConfig
                   , MonadIO
                   )

type WithSimulation m
  = (MonadIO m, MonadState SimSystem m, MonadReader SimConfig m)

instance EventGenerator Simulation where
  generateEvents g = do
    serie   <- _expDistribution <$> ask
    service <- _betaDistribution <$> ask
    return . map (flip (^.) _1) $ scanl (acumm serie service)
                                        (firstItem serie service)
                                        ([1 ..] :: [Integer])
   where
    firstItem serie service =
      let (arrival', g' ) = genArrival serie g
          (waiting', g'') = genServiceTime service g
      in  (Entity arrival' waiting' 0, g', g'')
    acumm serie service (Entity {..}, g', g'') _ =
      let (newArrival', h ) = genArrival serie g'
          (newWaiting , h') = genServiceTime service g''
          newArrival        = _arrival + newArrival'
      in  (Entity newArrival newWaiting 0, h, h')


zipWithDef :: [Integer] -> [Entity] -> [(Integer, Maybe Entity)]
zipWithDef [] [] = []
zipWithDef [] _  = []
zipWithDef _  [] = []
zipWithDef (x : xs) el@(e : es)
  | x == e ^. arrival = (x, Just e) : zipWithDef xs es
  | otherwise         = (x, Nothing) : zipWithDef xs el


queueEntity :: Entity -> Server -> Server
queueEntity e s = s & inQueue %~ Q.insert e

updateWaitingTimes :: Entity -> SimSystem -> SimSystem
updateWaitingTimes e s =
  let timeWaited      = e ^. serveTime - e ^. arrival
      waitSumUpdate   = (stats . waitingAmountSum) +~ timeWaited
      maxWaitUpdate   = (stats . maxWaitingTime) %~ (max timeWaited)
      countWaitUpdate = (stats . countWaiting) +~ (min 1 timeWaited)
  in  s & waitSumUpdate & maxWaitUpdate & countWaitUpdate


updateServiceTime :: SimSystem -> Entity -> SimSystem
updateServiceTime s e = if (e ^. serveTime + e ^. execTime) <= s ^. currentTime
  then
    let leaveServer = servers . inProcess .~ Nothing
        addToServed = entitiesServed %~ ((:) e)
    in  s & leaveServer & addToServed & updateWaitingTimes e
  else s


lookupQueue :: SimSystem -> SimSystem
lookupQueue s =
  let lookupQueue' = s ^. servers ^. inQueue . L.to Q.getMin
      checkTime e = if s ^. currentTime >= e ^. arrival
        then
          let getFromQueue = servers . inQueue %~ Q.deleteMin
              updateEntity = e & serveTime .~ s ^. currentTime
              putInServer  = servers . inProcess .~ Just updateEntity
          in  s & getFromQueue & putInServer
        else s
  in  maybe s checkTime lookupQueue'


runServer :: SimSystem -> SimSystem
runServer s =
  let serviceTime =
          s ^. servers . inProcess . L.to (maybe s (updateServiceTime s))
      updateQueue s' =
          s' ^. servers . inProcess . L.to (maybe (lookupQueue s') (const s'))
  in  serviceTime & updateQueue

-- Run an Event cycle of Event Time Unit
processEventCycle :: MonadState SimSystem m => (Integer, Maybe Entity) -> m ()
processEventCycle (currTime, maybeEntity) = modify $ \simState ->
  case maybeEntity of
    Nothing -> simState & runServerCycle currTime
    Just e ->
      let arrivedEntity  = servers %~ (queueEntity e)
          countCustomers = stats . customerAmount %~ (+ 1)
      in  simState & arrivedEntity & countCustomers & runServerCycle currTime


runServerCycle :: Integer -> SimSystem -> SimSystem
runServerCycle currTime s =
  let updateTime = currentTime .~ currTime
      queueS     = servers . inQueue . L.to (fromIntegral . Q.size)
      queueL s' = s' & (stats . queueLengthSum) +~ (s' ^. queueS)
      queueM s' = s' & stats . maxQueueLength %~ (max (s' ^. queueS))
      queueC s' = s' & (stats . countQueueNEmpty) +~ (min 1 (s' ^. queueS))
      updateStats = queueL . queueM . queueC
  in  s & updateTime & runServer & updateStats

-- Run program
executeSimulation :: SimConfig -> IO SimSystem
executeSimulation =
  fmap snd . runReaderT (runStateT (runSimulation simulation) initialSimState)

simulation :: (EventGenerator m, WithSimulation m) => m ()
simulation = liftIO newStdGen >>= generateEvents >>= processEvents . zipWithDef [1 ..]

processEvents :: WithSimulation m => [(Integer, Maybe Entity)] -> m ()
processEvents [] = pure ()
processEvents (x : xs) =
  whenM (continue $ x ^. _1) $ processEventCycle x >> processEvents xs

continue :: WithSimulation m => Integer -> m Bool
continue currTime = _maxRunningTime <$> ask >>= pure . (currTime <=)

initialSimState :: SimSystem
initialSimState = def
