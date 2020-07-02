module Simulation.InternalSpec
  ( spec
  )
where

import           Data.Default
import           Lens.Micro      as L
import           Protolude
import           Simulation
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "Run Simulation in Deterministic Context" $ do
    it "Run Successfull simulation with Perfect system in deterministic serie"
      $ do
          execSimSpec runSimSpec simConfigSpec >>= \result -> do
            result ^. entitiesServed . L.to length `shouldBe` 499
            result ^. currentTime `shouldBe` 999
            result ^. stats ^. maxWaitingTime `shouldBe` 0
            result ^. stats ^. customerAmount `shouldBe` 500
            result ^. stats ^. waitingAmountSum `shouldBe` 0
            result ^. stats ^. queueLengthSum `shouldBe` 0
            result ^. stats ^. maxQueueLength `shouldBe` 0

    it "Run Successfull simulation with Normal Waits and Normal Queues. Stable System"
      $ do
          execSimSpec runSimStableSpec simConfigSpec >>= \result -> do
            result ^. entitiesServed . L.to length `shouldSatisfy` (<499)
            result ^. currentTime `shouldBe` 997
            result ^. stats ^. maxWaitingTime `shouldSatisfy` (<50)
            result ^. stats ^. customerAmount `shouldBe` 250
            result ^. stats ^. waitingAmountSum `shouldSatisfy` (<1000)
            result ^. stats ^. queueLengthSum `shouldSatisfy` (<1000)
            result ^. stats ^. maxQueueLength `shouldSatisfy` (<200)

    it "Run Successfull simulation with Long Waits and Full Queues. Non Stable System"
      $ do
          execSimSpec runSimNonStableSpec simConfigSpec >>= \result -> do
            result ^. entitiesServed . L.to length `shouldSatisfy` (<499)
            result ^. currentTime `shouldBe` 999
            result ^. stats ^. maxWaitingTime `shouldSatisfy` (>100)
            result ^. stats ^. customerAmount `shouldBe` 500
            result ^. stats ^. waitingAmountSum `shouldSatisfy` (>10000)
            result ^. stats ^. queueLengthSum `shouldSatisfy` (>10000)
            result ^. stats ^. maxQueueLength `shouldSatisfy` (>100)


execSimSpec :: ( EventGenerator m
               , MonadIO m
               , MonadState SimSystem m
               , MonadReader SimConfig m
               , Default b
               , Functor f
               )
            => (m () -> StateT b (ReaderT r f) a)
            -> r
            -> f b
execSimSpec runSim = fmap snd . runReaderT (runStateT (runSim simulation) def)

simConfigSpec :: SimConfig
simConfigSpec = SimConfig 10000 (Beta 1 1) (Exponential 10)

newtype SimulationPerfectSpec a = SimulationPerfectSpec { runSimSpec :: StateT SimSystem (ReaderT SimConfig IO) a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadState SimSystem
                   , MonadReader SimConfig
                   , MonadIO
                   )

instance EventGenerator SimulationPerfectSpec where
  generateEvents _ = pure $ Entity <$> [1, 3 .. 1000] <*> pure 1 <*> pure 0

newtype SimulationStableSpec a = SimulationStableSpec { runSimStableSpec :: StateT SimSystem (ReaderT SimConfig IO) a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadState SimSystem
                   , MonadReader SimConfig
                   , MonadIO
                   )

instance EventGenerator SimulationStableSpec where
  generateEvents _ = do
    forM [1,5..1000] $ \idx -> do
      serviceTime <- liftIO . generate $ elements [1,2,3,4,5]
      pure $ Entity idx serviceTime 0

newtype SimulationNonStableSpec a = SimulationNonStableSpec { runSimNonStableSpec :: StateT SimSystem (ReaderT SimConfig IO) a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadState SimSystem
                   , MonadReader SimConfig
                   , MonadIO
                   )

instance EventGenerator SimulationNonStableSpec where
  generateEvents _ = do
    forM [1,3..1000] $ \idx -> do
      serviceTime <- liftIO . generate $ elements [1,5,10,30]
      pure $ Entity idx serviceTime 0

