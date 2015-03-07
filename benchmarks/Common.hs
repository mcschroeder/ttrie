{-# LANGUAGE DeriveGeneric #-}
module Common where

import BenchGen

import Control.Applicative
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import System.Random.MWC
import System.Random.MWC.CondensedTable
import Text.Printf

import qualified Control.Concurrent.STM.Map as TTrie
import qualified STMContainers.Map as STMCont
import qualified Data.HashMap.Strict as HashMap

data Op = Lookup | Insert | Delete
    deriving (Eq, Show, Generic)

instance NFData Op

config = Config
    { operations = mkTable [ (Lookup, 20)
                           , (Insert,  5)
                           , (Delete,  1)
                           ]
    , keys = \op -> case op of
        Lookup -> mkTable [(key,           1), (reuse key,            5)]
        Insert -> mkTable [(remember key, 10), (remember (reuse key), 1)]
        Delete -> mkTable [(forget key,    1), (forget (reuse key),  10)]
    }

key = do
    n <- liftGen $ genFromTable length
    T.pack <$> replicateM n (liftGen $ genFromTable alphabet)
  where
    length = mkTable $ zip [7..20] (repeat 1)
    alphabet = mkTable $ zip ['a'..'z'] (repeat 1)

sizes = mkTable $ [(1,10)] ++ zip [2..7] (repeat 5) ++ zip [8..14] (repeat 1)

genTransactions :: Int -> IO [[(Op, Text)]]
genTransactions numTransactions = do
    printf "Generating %d random transactions...\n" numTransactions
    gen <- create
    runGenerator gen $ do
        replicateM numTransactions $ do
            size <- liftGen $ genFromTable sizes
            generateOperations config size

split n [] = []
split n xs = let (ys,zs) = splitAt n xs in ys : split n zs



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
