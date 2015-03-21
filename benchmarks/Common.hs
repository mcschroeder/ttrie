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
import System.Environment
import System.Random.MWC
import System.Random.MWC.CondensedTable
import Text.Printf

import qualified Control.Concurrent.STM.Map as TTrie
import qualified STMContainers.Map as STMCont
import qualified Data.HashMap.Strict as HashMap

------------------------------------------------------------------------------

commonMain :: ([Int] -> Int -> CondensedTableV Int -> Config IO Op Key -> IO ()) -> IO ()
commonMain f = do
    (arg1:arg2:arg3:arg4:args) <- getArgs
    printf "threads = %s\nnumTransactions = %s\nsizes = %s\nops = %s\n" arg1 arg2 arg3 arg4
    let threads = read arg1
        numTransactions = read arg2
        sizes = mkTable $ read arg3
        (inserts,lookups,deletes) = read arg4
        config = Config
            { operations = mkTable [ (Insert, inserts)
                                   , (Lookup, lookups)
                                   , (Delete, deletes)
                                   ]
            , keys = \op -> case op of
                Insert -> mkTable [(remember key,       1)]
                Lookup -> mkTable [(reuse key,          1)]
                Delete -> mkTable [(forget (reuse key), 1)]
            }
    withArgs args $ f threads numTransactions sizes config

genTransactions :: Int -> Int -> CondensedTableV Int -> Config IO Op Key -> IO ([Key],[Transaction])
genTransactions numPrefill numTransactions sizes config = do
    printf "Generating %d random keys to prefill and %d random transactions...\n" numPrefill numTransactions
    gen <- create
    runGenerator gen $ do
        ks <- replicateM numPrefill (remember key)
        txs <- replicateM numTransactions $ do
            size <- liftGen $ genFromTable sizes
            generateOperations config size
        return (ks,txs)

bthreads :: [Int] -> Int -> [Transaction] -> ([[Transaction]] -> [Benchmark]) -> Benchmark
bthreads threads numTransactions txs bs = bgroup ""
    $ flip map threads
    $ \n -> env (return $ split (numTransactions `div` n) txs)
    $ \ ~ops -> bgroup (printf "%d/%d" n (numTransactions `div` n))
    $ bs ops

runBench :: [[Transaction]] -> (Op -> Key -> c -> STM ()) -> c -> IO ()
runBench ops f c = void $ mapConcurrently (mapM_ txEval) ops
  where txEval = atomically . mapM_ (\(op,k) -> f op k c)

runStats :: String -> [[Transaction]] -> (Op -> Key -> c -> STM ()) -> c -> IO ()
runStats name ops f c = do
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

split :: Int -> [a] -> [[a]]
split n [] = []
split n xs = let (ys,zs) = splitAt n xs in ys : split n zs

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

------------------------------------------------------------------------------
-- Evaluation functions

instance (NFData k, NFData v) => NFData (TTrie.Map k v)

ttriePrefill ks = do
  m <- atomically $ TTrie.empty
  forM_ ks $ \k -> atomically $ TTrie.insert k () m
  return m

ttrieEval Lookup k = void . TTrie.lookup k
ttrieEval Insert k = TTrie.insert k ()
ttrieEval Delete k = TTrie.delete k

instance (NFData k, NFData v) => NFData (STMCont.Map k v)

stmcontPrefill ks = do
  m <- atomically $ STMCont.new
  forM_ ks $ \k -> atomically $ STMCont.insert () k m
  return m

stmcontEval Lookup k = void . STMCont.lookup k
stmcontEval Insert k = STMCont.insert () k
stmcontEval Delete k = STMCont.delete k

instance (NFData a) => NFData (TVar a)

hashmapPrefill ks = do
  elems <- forM ks (\k -> newTVarIO () >>= \v -> return (k,v))
  m <- newTVarIO $ HashMap.fromList elems
  return m

hashmapEval Lookup k m = do
  v <- HashMap.lookup k <$> readTVar m
  case v of
    Just v -> void $ readTVar v
    Nothing -> return ()
hashmapEval Insert k m = do
  v <- newTVar ()
  modifyTVar' m (HashMap.insert k v)
hashmapEval Delete k m = modifyTVar' m (HashMap.delete k)
