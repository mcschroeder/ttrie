module Main where

import BenchGen
import Common
import System.Environment
import Text.Printf
import Criterion.Main
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Control.Monad

main :: IO ()
main = do
    (arg1:args) <- getArgs
    printf "numPrefill = %s\n" arg1
    let numPrefill = read arg1
    withArgs args $ commonMain $ \threads numTransactions sizes config -> do
        (ks,txs) <- genTransactions numPrefill numTransactions sizes config
        runBenchmarks1 ks txs numTransactions threads
        collectRetryStats1 ks txs numTransactions threads

runBenchmarks1 :: [Key] -> [Transaction] -> Int -> [Int] -> IO ()
runBenchmarks1 ks txs numTransactions threads = defaultMain
    [ env (prefill ks) $ \ ~(hashmap,stmcont,ttrie) ->
        bthreads threads numTransactions txs $ \ops ->
        [ bench "unordered-containers" $ whnfIO $ runBench ops hashmapEval hashmap
        , bench "stm-containers"       $ whnfIO $ runBench ops stmcontEval stmcont
        , bench "ttrie"                $ whnfIO $ runBench ops ttrieEval ttrie
        ]
    ]

collectRetryStats1 :: [Key] -> [Transaction] -> Int -> [Int] -> IO ()
collectRetryStats1 ks txs numTransactions threads = do
    cs@(hashmap,stmcont,ttrie) <- prefill ks
    evaluate (rnf cs)
    forM_ threads $ \n -> do
        let ops = split (numTransactions `div` n) txs
            str = printf "%d/%d" n (numTransactions `div` n)
        runStats (str++"/unordered-containers") ops hashmapEval hashmap
        runStats (str++"/stm-containers")       ops stmcontEval stmcont
        runStats (str++"/ttrie")                ops ttrieEval ttrie

prefill ks = do
    printf "Creating containers with %d keys prefilled...\n" (length ks)
    hashmap <- hashmapPrefill ks
    stmcont <- stmcontPrefill ks
    ttrie <- ttriePrefill ks
    return $! (hashmap,stmcont,ttrie)
