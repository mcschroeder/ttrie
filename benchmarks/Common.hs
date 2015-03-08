{-# LANGUAGE DeriveGeneric #-}
module Common where

import BenchGen

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.Stats
import Control.DeepSeq
import Control.Monad
import Criterion.Main
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import System.Random.MWC
import System.Random.MWC.CondensedTable
import Text.Printf

import qualified Control.Concurrent.STM.Map as TTrie
import qualified STMContainers.Map as STMCont
import qualified Data.HashMap.Strict as HashMap

------------------------------------------------------------------------------

data Op = Lookup | Insert | Delete
    deriving (Eq, Show, Generic)

instance NFData Op

key :: Generator IO k Text
key = do
    n <- liftGen $ genFromTable length
    T.pack <$> replicateM n (liftGen $ genFromTable alphabet)
  where
    length = mkTable $ zip [7..20] (repeat 1)
    alphabet = mkTable $ zip ['a'..'z'] (repeat 1)

------------------------------------------------------------------------------

runAll :: [Int] -> Int -> CondensedTableV Int -> Config IO Op Text -> IO ()
runAll numThreads numTransactions sizes config = do
    txs <- genTransactions numTransactions sizes config
    runBenchmarks txs numTransactions numThreads
    collectRetryStats txs numTransactions numThreads

genTransactions :: Int -> CondensedTableV Int -> Config IO Op Text -> IO [[(Op, Text)]]
genTransactions numTransactions sizes config = do
    printf "Generating %d random transactions...\n" numTransactions
    gen <- create
    runGenerator gen $ do
        replicateM numTransactions $ do
            size <- liftGen $ genFromTable sizes
            generateOperations config size

runBenchmarks :: [[(Op, Text)]] -> Int -> [Int] -> IO ()
runBenchmarks txs numTransactions numThreads =
    defaultMain $! flip map numThreads
         $ \n -> env (return $! split (numTransactions `div` n) txs)
         $ \ ~ops -> bgroup (printf "%d/%d" n (numTransactions `div` n))
         [ bench "unordered-containers" $ whnfIO $ run ops hashmapEval =<< newTVarIO HashMap.empty
         , bench "stm-containers"       $ whnfIO $ run ops stmcontEval =<< STMCont.newIO
         , bench "ttrie"                $ whnfIO $ run ops ttrieEval =<< atomically TTrie.empty
         ]
  where
    run ops f c = mapConcurrently (mapM_ txEval) ops
      where txEval = atomically . mapM_ (\(op,k) -> f op k c)


collectRetryStats :: [[(Op, Text)]] -> Int -> [Int] -> IO ()
collectRetryStats txs numTransactions numThreads =
    flip mapM_ numThreads $ \n -> do
        let ops = split (numTransactions `div` n) txs
            str = printf "%d/%d" n (numTransactions `div` n)
        run (str++"/unordered-containers") ops hashmapEval =<< newTVarIO HashMap.empty
        run (str++"/stm-containers")       ops stmcontEval =<< STMCont.newIO
        run (str++"/ttrie")                ops ttrieEval =<< atomically TTrie.empty
  where
    run name ops f c = do
        printf "collecting retry statistics for %s\n" name
        mapConcurrently (mapM_ txEval) ops
        printStats name
      where
        txEval = atomically' name . mapM_ (\(op,k) -> f op k c)
        atomically' = trackSTMConf defaultTrackSTMConf
                            { tryThreshold = Nothing
                            , globalTheshold = Nothing
                            }
        printStats name = do
            stats <- getSTMStats
            let Just (commits, retries) = Map.lookup name stats
            printf "Commits\t%d\nRetries\t%d\n\n" commits retries

split n [] = []
split n xs = let (ys,zs) = splitAt n xs in ys : split n zs

------------------------------------------------------------------------------
-- Evaluation functions

ttrieEval Lookup k = void . TTrie.lookup k
ttrieEval Insert k = TTrie.insert k ()
ttrieEval Delete k = TTrie.delete k

stmcontEval Lookup k = void . STMCont.lookup k
stmcontEval Insert k = STMCont.insert () k
stmcontEval Delete k = STMCont.delete k

hashmapEval Lookup k m = do
    v <- HashMap.lookup k <$> readTVar m
    case v of
        Nothing -> return ()
        Just v -> readTVar v >> return ()
hashmapEval Insert k m = do
    v <- newTVar ()
    modifyTVar' m (HashMap.insert k v)
hashmapEval Delete k m = modifyTVar' m (HashMap.delete k)
