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

runAll :: [Int] -> Int -> Int -> CondensedTableV Int -> Config IO Op Key -> IO ()
runAll numThreads numPrefill numTransactions sizes config = do
    (ks,txs) <- genTransactions numPrefill numTransactions sizes config
    runBenchmarks ks txs numTransactions numThreads
    collectRetryStats ks txs numTransactions numThreads

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

runBenchmarks :: [Key] -> [Transaction] -> Int -> [Int] -> IO ()
runBenchmarks ks txs numTransactions numThreads =
    defaultMain $! flip map numThreads
         $ \n -> env (mkEnv ks txs numTransactions n)
         $ \ ~(hashmap,stmcont,ttrie,ops) -> bgroup (printf "%d/%d" n (numTransactions `div` n))
         [ bench "unordered-containers" $ whnfIO $ run ops hashmapEval hashmap
         , bench "stm-containers"       $ whnfIO $ run ops stmcontEval stmcont
         , bench "ttrie"                $ whnfIO $ run ops ttrieEval ttrie
         ]
  where
    run ops f c = mapConcurrently (mapM_ txEval) ops
      where txEval = atomically . mapM_ (\(op,k) -> f op k c)

collectRetryStats :: [Key] -> [Transaction] -> Int -> [Int] -> IO ()
collectRetryStats ks txs numTransactions numThreads =
    flip mapM_ numThreads $ \n -> do
        (hashmap,stmcont,ttrie,ops) <- mkEnv ks txs numTransactions n
        let str = printf "%d/%d" n (numTransactions `div` n)
        run (str++"/unordered-containers") ops hashmapEval hashmap
        run (str++"/stm-containers")       ops stmcontEval stmcont
        run (str++"/ttrie")                ops ttrieEval ttrie
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

mkEnv ks txs numTransactions n = do
  printf "Creating new containers with %d keys prefilled...\n" (length ks)
  hashmap <- hashmapPrefill ks
  stmcont <- stmcontPrefill ks
  ttrie <- ttriePrefill ks
  ops <- return $! split (numTransactions `div` n) txs
  return $! (hashmap,stmcont,ttrie,ops)

split n [] = []
split n xs = let (ys,zs) = splitAt n xs in ys : split n zs

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
