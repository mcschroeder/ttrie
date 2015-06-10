{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import BenchGen
import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.Stats
import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad
import Control.Monad.IO.Class
import CriterionPlus
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import GHC.Stats
import System.Environment
import System.Mem
import System.Random.MWC
import System.Random.MWC.CondensedTable
import Text.Printf

import qualified Control.Concurrent.STM.Map as TTrie
import qualified STMContainers.Map as STMCont
import qualified Data.HashMap.Strict as HashMap

------------------------------------------------------------------------------

main :: IO ()
main = do
    (arg1:arg2:arg3:arg4:arg5:args) <- getArgs
    printf "threads = %s\n"         arg1
    printf "numTransactions = %s\n" arg2
    printf "sizes = %s\n"           arg3
    printf "numPrefill = %s\n"      arg4
    printf "ops = %s\n"             arg5
    let threads            = read arg1 :: [Int]
        numTransactions    = read arg2
        sizes              = mkTable $ read arg3
        numPrefill         = read arg4
        (ins,upd,look,del) = read arg5
        config = Config
            { operations = mkTable [ (Insert, ins + upd)
                                   , (Lookup, look)
                                   , (Delete, del)
                                   ]
            , keys = \op -> case op of
                Insert -> mkTable [(remember key, ins), (remember (reuse key), upd)]
                Lookup -> mkTable [(reuse key, 1)]
                Delete -> mkTable [(forget (reuse key), 1)]
            }

    (ks,txs) <- genTransactions numPrefill numTransactions sizes config
    withArgs args $ benchmark
                  $ standoff (T.pack arg5)
                  $ mapM_ (supergroup ks txs numTransactions)
                  $ threads

supergroup :: [Key] -> [Transaction] -> Int -> Int -> Standoff ()
supergroup ks txs numTransactions t =
    group (T.pack $ printf "%d/%d" t n) $ do
        runAll "unordered-containers" hashmapPrefill hashmapEval
        runAll "stm-containers"       stmcontPrefill stmcontEval
        runAll "ttrie"                ttriePrefill  ttrieEval
  where
    n = numTransactions `div` t
    ops = split n txs
    runAll name prefill eval = do
        runTime name prefill eval ks ops
        liftIO $ runRetries (printf "%d/%d/%s" t n name) prefill eval ks ops
        liftIO $ runAlloc prefill eval ks ops

split :: Int -> [a] -> [[a]]
split n [] = []
split n xs = let (ys,zs) = splitAt n xs in ys : split n zs

------------------------------------------------------------------------------

type EvalFunc c = Op -> Key -> c -> STM ()
type PrefillFunc c = [Key] -> IO c

runTime :: String -> PrefillFunc c -> EvalFunc c -> [Key] -> [[Transaction]] -> Standoff ()
runTime name prefill eval ks ops = subject (T.pack name) $ do
    pause
    c <- liftIO $ prefill ks
    continue
    liftIO $ run_ atomically ops eval c

runRetries :: String -> PrefillFunc c -> EvalFunc c -> [Key] -> [[Transaction]] -> IO ()
runRetries name prefill eval ks ops = do
    printf "commits: "
    c <- prefill ks
    let atomically' = trackSTMConf defaultTrackSTMConf
                        { tryThreshold = Nothing
                        , globalTheshold = Nothing
                        }
    run_ (atomically' name) ops eval c
    stats <- getSTMStats
    let Just (commits, retries) = Map.lookup name stats
    printf "%d\nretries: %d\n" commits retries

runAlloc :: PrefillFunc c -> EvalFunc c -> [Key] -> [[Transaction]] -> IO ()
runAlloc prefill eval ks ops = do
    printf "allocated bytes: "
    c <- prefill ks
    performGC
    before <- getGCStats
    run_ atomically ops eval c
    performGC
    after <- getGCStats
    let bytes = bytesAllocated after - bytesAllocated before
    printf "%d\n" bytes

run_ :: (STM () -> IO ()) -> [[Transaction]] -> EvalFunc c -> c -> IO ()
run_ atom ops f c = void $ mapConcurrently (mapM_ txEval) ops
  where txEval = atom . mapM_ (\(op,k) -> f op k c)

------------------------------------------------------------------------------

type Key = Text
type Transaction = [(Op,Key)]

data Op = Lookup | Insert | Delete
    deriving (Eq, Show, Generic)

instance NFData Op

key :: Generator IO k Key
key = do
    n <- liftGen $ genFromTable length
    T.pack <$> replicateM n (liftGen $ genFromTable alphabet)
  where
    length = mkTable $ zip [7..20] (repeat 1)
    alphabet = mkTable $ zip ['a'..'z'] (repeat 1)

genTransactions :: Int -> Int -> CondensedTableV Int -> Config IO Op Key -> IO ([Key],[Transaction])
genTransactions numPrefill numTransactions sizes config = do
    gen <- create
    runGenerator gen $ do
        liftIO $ printf "Generating %d random keys to prefill...\n" numPrefill
        ks <- replicateM numPrefill (remember key)
        liftIO $ printf "Generating %d random transactions...\n" numTransactions
        txs <- replicateM numTransactions $ do
            size <- liftGen $ genFromTable sizes
            generateOperations config size
        return (ks,txs)

------------------------------------------------------------------------------
-- Evaluation and Prefill functions

instance (NFData k, NFData v) => NFData (TTrie.Map k v)

ttriePrefill :: PrefillFunc (TTrie.Map Key ())
ttriePrefill ks = do
  m <- atomically $ TTrie.empty
  forM_ ks $ \k -> atomically $ TTrie.insert k () m
  evaluate (rnf m)
  return m

ttrieEval :: EvalFunc (TTrie.Map Key ())
ttrieEval Lookup k = void . TTrie.lookup k
ttrieEval Insert k = TTrie.insert k ()
ttrieEval Delete k = TTrie.delete k

instance (NFData k, NFData v) => NFData (STMCont.Map k v)

stmcontPrefill :: PrefillFunc (STMCont.Map Key ())
stmcontPrefill ks = do
  m <- atomically $ STMCont.new
  forM_ ks $ \k -> atomically $ STMCont.insert () k m
  evaluate (rnf m)
  return m

stmcontEval :: EvalFunc (STMCont.Map Key ())
stmcontEval Lookup k = void . STMCont.lookup k
stmcontEval Insert k = STMCont.insert () k
stmcontEval Delete k = STMCont.delete k

instance (NFData a) => NFData (TVar a)

hashmapPrefill :: PrefillFunc (TVar (HashMap.HashMap Key ()))
hashmapPrefill ks = do
  let m = HashMap.fromList $ zip ks $ repeat ()
  evaluate (rnf m)
  newTVarIO m

hashmapEval :: EvalFunc (TVar (HashMap.HashMap Key ()))
hashmapEval Lookup k m = void $ HashMap.lookup k <$> readTVar m
hashmapEval Insert k m = modifyTVar' m (HashMap.insert k ())
hashmapEval Delete k m = modifyTVar' m (HashMap.delete k)
