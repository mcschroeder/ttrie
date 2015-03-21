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
        runBenchmarks2 ks txs numTransactions threads
        collectRetryStats2 ks txs numTransactions threads

runBenchmarks2 :: [Key] -> [Transaction] -> Int -> [Int] -> IO ()
runBenchmarks2 ks txs numTransactions threads = defaultMain
    [ bench "prefill" $ whnfIO $ prefill ks
    , bthreads threads numTransactions txs $ \ops ->
        [ bench "unordered-containers" $ whnfIO $ prefill ks >>= \(hashmap,_,_) -> runBench ops hashmapEval hashmap
        , bench "stm-containers"       $ whnfIO $ prefill ks >>= \(_,stmcont,_) -> runBench ops stmcontEval stmcont
        , bench "ttrie"                $ whnfIO $ prefill ks >>= \(_,_,ttrie)   -> runBench ops ttrieEval ttrie
        ]
    ]

collectRetryStats2 :: [Key] -> [Transaction] -> Int -> [Int] -> IO ()
collectRetryStats2 ks txs numTransactions threads = do
    cs@(hashmap,stmcont,ttrie) <- prefill ks
    evaluate (rnf cs)
    forM_ threads $ \n -> do
        let ops = split (numTransactions `div` n) txs
            str = printf "%d/%d" n (numTransactions `div` n)
        runStats (str++"/unordered-containers") ops hashmapEval hashmap
        runStats (str++"/stm-containers")       ops stmcontEval stmcont
        runStats (str++"/ttrie")                ops ttrieEval ttrie

prefill ks = do
    hashmap <- hashmapPrefill ks
    stmcont <- stmcontPrefill ks
    ttrie <- ttriePrefill ks
    let cs = (hashmap,stmcont,ttrie)
    evaluate (rnf cs)
    return cs
