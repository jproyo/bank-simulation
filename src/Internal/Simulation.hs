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
  { _arrival  :: Integer
  , _waiting  :: Integer
  , _waitDecr :: Integer
  } deriving Show


data SimState = SimState
  { _currentTime :: Integer
  , _maxTime     :: Integer
  , _servers     :: Server
  , _solution    :: SolutionOutput
  }

data SolutionOutput = SolutionOutput
  { _customerAmount   :: Integer
  , _waitingAmountSum :: Integer
  , _maxWaitingTime   :: Integer
  , _queueLengthSum   :: Integer
  , _maxQueueLength   :: Integer
  } deriving (Generic, Default, Show)

makeLenses ''Entity
makeLenses ''SolutionOutput
makeLenses ''SimState
makeLenses ''Server

instance Eq Entity where
  entA == entB = entA ^. arrival == entB ^. arrival

instance Ord Entity where
  entA <= entB = entA ^. arrival <= entB ^. arrival

arrivalTime :: RandomGen g => g -> (Integer, g)
arrivalTime g =
  let (v, g') = randomR @Float (0.0, 1.0) g
  in  (round ((-100) * log (1 - v)), g')


waitingTime :: RandomGen g => g -> (Integer, g)
waitingTime g =
  let (v, g') = randomR @Float (0.0, 1.0) g
  in  ( round
        (200 * (v ^ ((2 - 1) :: Integer)) * ((1 - v) ^ ((5 - 1) :: Integer)))
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

runSimulation :: IO SimState
runSimulation =
  newStdGen
    >>= return
    .   either identity identity
    .   foldM process initialSimState
    .   zipWithDef [1 ..]
    .   map (\(e, _, _) -> e)
    .   generateEvents

initialSimState :: SimState
initialSimState = SimState { _currentTime = 0
                           , _servers     = Server Q.empty Nothing
                           , _solution    = def
                           , _maxTime     = 60 * 60 * 8 -- Time in Seconds for 8 hours work
                           }

queueEntity :: Entity -> Server -> Server
queueEntity e s = s & inQueue %~ Q.insert e


updateWaiting :: Server -> Entity -> Server
updateWaiting s e =
  let newE = if e ^. waiting > 0 then e & waitDecr %~ (+ 1) else e
  in  if newE ^. waitDecr == newE ^. waiting
        then s & inProcess .~ Nothing
        else s & inProcess .~ Just newE


lookupQueue :: Integer -> Server -> Server
lookupQueue currTime s =
  let lookupQueue' = s ^. inQueue . L.to Q.getMin
      checkTime e = if currTime >= e ^. arrival
        then s & inQueue %~ Q.deleteMin & inProcess .~ Just e
        else s & inProcess .~ Nothing
  in  maybe s checkTime lookupQueue'


runServer :: Integer -> Server -> Server
runServer c s =
  s ^. inProcess . L.to (maybe (lookupQueue c s) (updateWaiting s))

process :: SimState -> (Integer, Maybe Entity) -> Either SimState SimState
process s (currTime, Nothing) | currTime > s ^. maxTime = Left s
                              | otherwise = s & currentTime .~ currTime & Right
process s (currTime, Just e)
  | currTime > s ^. maxTime
  = Left s
  | otherwise
  = let updateTime    = currentTime .~ currTime
        pushEntity    = servers %~ (queueEntity e)
        processServer = servers %~ runServer (s ^. currentTime)
        customers     = solution . customerAmount %~ (+ 1)
    in  s & updateTime & pushEntity & processServer & customers & Right


--newtype Simuation a = Simulation { runSimulation :: [Entity a] }
