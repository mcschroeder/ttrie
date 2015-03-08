module Main where

import BenchGen
import Common

main :: IO ()
main = runAll threads n sizes config
  where
    threads = [1,2,4,6,8,12,16,32,40,52,64,80,128]
    n       = 400000
    sizes   = mkTable $ [(1,10)] ++ zip [2..7] (repeat 5) ++ zip [8..14] (repeat 1)
    config  = Config { operations = mkTable [(Insert,1)]
                     , keys = \_ -> mkTable [(key,1)]
                     }
