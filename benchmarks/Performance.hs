module Main where

import Common

import Control.Concurrent.Async
import Control.Concurrent.STM
import Criterion.Main
import Text.Printf

import qualified Control.Concurrent.STM.Map as TTrie
import qualified STMContainers.Map as STMCont
import qualified Data.HashMap.Strict as HashMap

main :: IO ()
main = do

    let numTransactions = 400000
        numThreads      = [1,2,4,6,8,12,16,32,40,52,64,80,128]

    txs <- genTransactions numTransactions

    defaultMain $! flip map numThreads
         $ \n -> env (return $! split (numTransactions `div` n) txs)
         $ \ ~ops -> bgroup (printf "%d/%d" n (numTransactions `div` n))
         [ bench "unordered-containers" $ whnfIO $
            run ops hashmapEval =<< newTVarIO HashMap.empty
         , bench "stm-containers" $ whnfIO $
            run ops stmcontEval =<< STMCont.newIO
         , bench "ttrie" $ whnfIO $
            run ops ttrieEval =<< atomically TTrie.empty
         ]

run ops f c = mapConcurrently (mapM_ txEval) ops
    where txEval = atomically . mapM_ (\(op,k) -> f op k c)
