module Main where

import BenchGen
import Common
import System.Environment
import Text.Printf
import Criterion.Main
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad

import qualified Control.Concurrent.STM.Map as TTrie
import qualified STMContainers.Map as STMCont
import qualified Data.HashMap.Strict as HashMap

main :: IO ()
main = commonMain $ \threads numTransactions sizes config -> do
    (_,txs) <- genTransactions 0 numTransactions sizes config
    runBenchmarks0 txs numTransactions threads
    collectRetryStats0 txs numTransactions threads

runBenchmarks0 :: [Transaction] -> Int -> [Int] -> IO ()
runBenchmarks0 txs numTransactions threads = defaultMain
    [ bthreads threads numTransactions txs $ \ops ->
        [ bench "unordered-containers" $ whnfIO $ runBench ops hashmapEval =<< newTVarIO HashMap.empty
        , bench "stm-containers"       $ whnfIO $ runBench ops stmcontEval =<< STMCont.newIO
        , bench "ttrie"                $ whnfIO $ runBench ops ttrieEval =<< atomically TTrie.empty
        ]
    ]

collectRetryStats0 :: [Transaction] -> Int -> [Int] -> IO ()
collectRetryStats0 txs numTransactions threads =
    forM_ threads $ \n -> do
        let ops = split (numTransactions `div` n) txs
            str = printf "%d/%d" n (numTransactions `div` n)
        runStats (str++"/unordered-containers") ops hashmapEval =<< newTVarIO HashMap.empty
        runStats (str++"/stm-containers")       ops stmcontEval =<< STMCont.newIO
        runStats (str++"/ttrie")                ops ttrieEval =<< atomically TTrie.empty


