module Main where

import BenchGen
import Common
import System.Environment
import Text.Printf

main :: IO ()
main = do
    (arg1:arg2:arg3:arg4:arg5:args) <- getArgs
    printf "threads = %s\nnumPrefill = %s\nnumTransactions = %s\nsizes = %s\nops = %s\n" arg1 arg2 arg3 arg4 arg5
    let threads = read arg1
        numPrefill = read arg2
        numTransactions = read arg3
        sizes = mkTable $ read arg4
        (inserts,lookups,deletes) = read arg5
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
    withArgs args $ runAll threads numPrefill numTransactions sizes config
