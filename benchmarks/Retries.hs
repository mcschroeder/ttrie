module Main where

import Common

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.Stats
import Text.Printf

import qualified Control.Concurrent.STM.Map as TTrie
import qualified STMContainers.Map as STMCont
import qualified Data.HashMap.Strict as HashMap

main :: IO ()
main = do

    let numTransactions = 400000
        numThreads      = [1,2,4,6,8,12,16,32,40,52,64,80,128]

    txs <- genTransactions numTransactions

    flip mapM_ numThreads $ \n -> do
        let ops = split (numTransactions `div` n) txs
            str = printf "%d/%d" n (numTransactions `div` n)
        run (str++"/unordered-containers") ops hashmapEval =<< newTVarIO HashMap.empty
        run (str++"/stm-containers") ops stmcontEval =<< STMCont.newIO
        run (str++"/ttrie") ops ttrieEval =<< atomically TTrie.empty

    dumpSTMStats


run name ops f c = putStrLn name >> mapConcurrently (mapM_ txEval) ops
    where txEval = atomically' name . mapM_ (\(op,k) -> f op k c)

atomically' = trackSTMConf defaultTrackSTMConf
                    { tryThreshold = Nothing
                    , globalTheshold = Nothing
                    }
