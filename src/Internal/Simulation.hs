{-# LANGUAGE AllowAmbiguousTypes #-}
module Internal.Simulation where

import           Data.Default
import qualified Data.PQueue.Min as Q
import           GHC.Show
import           Lens.Micro      as L
import           Lens.Micro.TH
import           Protolude       as P
import           System.Random


data Algo = Algo

class Num a => TimeGen t a where
  type Unit a :: *
  time :: t -> Unit a

instance TimeGen Algo Integer where
  type Unit Integer = Integer
  time Algo = 1

data Server = Server
  { _inQueue   :: Q.MinQueue Entity
  , _inProcess :: Maybe Entity
  }

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
  , _maxTime        :: Integer
  , _servers        :: Server
  , _entitiesServed :: [Entity]
  , _stats          :: SimStats
  } deriving Show

data SimStats = SimStats
  { _customerAmount   :: Integer
  , _waitingAmountSum :: Integer
  , _maxWaitingTime   :: Integer
  , _countWaiting     :: Integer
  , _queueLengthSum   :: Integer
  , _maxQueueLength   :: Integer
  } deriving (Generic, Default, Show)

makeLenses ''Entity
makeLenses ''SimStats
makeLenses ''SimSystem
makeLenses ''Server

instance Eq Entity where
  entA == entB = entA ^. arrival == entB ^. arrival

instance Ord Entity where
  entA <= entB = entA ^. arrival <= entB ^. arrival

arrivalTime :: RandomGen g => g -> (Integer, g)
arrivalTime g =
  let (v, g') = randomR @Float (0.0, 1.0) g
  in  (round ((-100) * log (1-v)), g')


waitingTime :: RandomGen g => g -> (Integer, g)
waitingTime g =
  let (v, g') = randomR @Float (0.0, 1.0) g
  in  ( round
        (200 * (v ^ ((5 - 1) :: Integer)) * ((1 - v) ^ ((1 - 1) :: Integer)))
      , g'
      )

generateEvents :: RandomGen g => g -> [(Entity, g, g)]
generateEvents g = scanl acumm firstItem ([1 ..] :: [Integer])
 where
  firstItem =
    let (arrival', g' ) = arrivalTime g
        (waiting', g'') = waitingTime g
    in  (Entity arrival' waiting' 0, g', g'')
  acumm (Entity {..}, g', g'') _ =
    let (newArrival', h ) = arrivalTime g'
        (newWaiting , h') = waitingTime g''
        newArrival        = _arrival + newArrival'
    in  (Entity newArrival newWaiting 0, h, h')


zipWithDef :: [Integer] -> [Entity] -> [(Integer, Maybe Entity)]
zipWithDef [] [] = []
zipWithDef [] _  = []
zipWithDef _  [] = []
zipWithDef (x : xs) el@(e : es)
  | x == e ^. arrival = (x, Just e) : zipWithDef xs es
  | otherwise         = (x, Nothing) : zipWithDef xs el

runSimulation :: IO SimSystem
runSimulation =
  newStdGen
    >>= fmap snd
    .   flip runStateT initialSimState
    .   processEvents
    .   zipWithDef [1 ..]
    .   map (flip (^.) _1)
    .   generateEvents


processEvents :: (MonadIO m, MonadState SimSystem m) => [(Integer, Maybe Entity)] -> m ()
processEvents []       = pure ()
processEvents (x : xs) = whenM (continue $ x^._1) $
                            processEventCycle x >> processEvents xs

continue :: MonadState SimSystem m => Integer -> m Bool
continue currTime = get >>= pure . (currTime<=) . flip (^.) maxTime

initialSimState :: SimSystem
initialSimState = SimSystem { _currentTime = 0
                            , _servers     = Server Q.empty Nothing
                            , _stats       = def
                            , _entitiesServed = []
                            , _maxTime     = 60 * 60 * 8 -- Time in Seconds for 8 hours work
                            }

queueEntity :: Entity -> Server -> Server
queueEntity e s = s & inQueue %~ Q.insert e

updateWaitingTimes :: Entity -> SimSystem -> SimSystem
updateWaitingTimes e s = let timeWaited  = e^.serveTime - e^.arrival
                             waitSumUpdate = (stats.waitingAmountSum) +~ timeWaited
                             maxWaitUpdate = (stats.maxWaitingTime) %~ (max timeWaited)
                             countWaitUpdate = (stats.countWaiting) +~ (min 1 timeWaited)
                        in s & waitSumUpdate & maxWaitUpdate & countWaitUpdate


updateServiceTime :: SimSystem -> Entity -> SimSystem
updateServiceTime s e = if (e^.serveTime + e^.execTime) <= s^.currentTime
                         then
                           let leaveServer = servers . inProcess .~ Nothing
                               addToServed = entitiesServed %~ ((:) e)
                            in s & leaveServer & addToServed & updateWaitingTimes e
                         else s


lookupQueue :: SimSystem -> SimSystem
lookupQueue s =
  let lookupQueue' = s ^. servers ^. inQueue . L.to Q.getMin
      checkTime e = if s ^. currentTime >= e ^. arrival
        then
          let getFromQueue = servers . inQueue %~ Q.deleteMin
              updateEntity = e & serveTime .~ s^.currentTime
              putInServer  = servers . inProcess .~ Just updateEntity
           in s & getFromQueue & putInServer -- & flip updateServiceTime updateEntity
        else s
  in  maybe s checkTime lookupQueue'


runServer :: SimSystem -> SimSystem
runServer s = let serviceTime = s ^. servers . inProcess . L.to (maybe s (updateServiceTime s))
                  updateQueue s' = s' ^. servers . inProcess . L.to (maybe (lookupQueue s') (const s'))
               in serviceTime & updateQueue


processEventCycle :: MonadState SimSystem m => (Integer, Maybe Entity) -> m ()
processEventCycle (currTime, maybeEntity) = modify $ \simState ->
  case maybeEntity of
    Nothing -> simState & runServerCycle currTime
    Just e ->
      let arrivedEntity = servers %~ (queueEntity e)
          countCustomers = stats . customerAmount %~ (+ 1)
       in simState & arrivedEntity & countCustomers & runServerCycle currTime


runServerCycle :: Integer -> SimSystem -> SimSystem
runServerCycle currTime s =
  let updateTime    = currentTime .~ currTime
      queueS        = servers . inQueue . L.to (fromIntegral . Q.size)
      queueL s' = s' & (stats. queueLengthSum) +~ (s' ^. queueS)
      queueM s' = s' & stats . maxQueueLength %~ (max (s' ^. queueS))
      updateStats = queueL . queueM
   in s & updateTime & runServer & updateStats


