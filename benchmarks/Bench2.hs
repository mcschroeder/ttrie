module Main where

import BenchGen
import Common

main :: IO ()
main = runAll threads n sizes config
  where
    threads = [1,2,4,6,8,12,16,32,40,52,64,80,128]
    n       = 400000
    sizes   = mkTable $ [(1,1)] ++ zip [2..7] (repeat 5) ++ zip [8..14] (repeat 10)
    config  = Config
        { operations = mkTable [ (Lookup, 10)
                               , (Insert, 10)
                               , (Delete,  2)
                               ]
        , keys = \op -> case op of
            Lookup -> mkTable [(key,           1), (reuse key,           50)]
            Insert -> mkTable [(remember key, 10), (remember (reuse key), 8)]
            Delete -> mkTable [(forget key,    1), (forget (reuse key), 100)]
        }
