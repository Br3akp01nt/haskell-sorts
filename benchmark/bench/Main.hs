module Main where

import           Control.Monad      (replicateM)
import           Criterion.Main     (Benchmark, bench, defaultMain, nf, env)
import           Data.Ord.Heapsort  (heapsort)
import           Data.Ord.Mergesort (mergesort)
import           Data.Ord.Quicksort (quicksort)
import           System.Random      (randomRIO)


main :: IO ()
main = defaultMain $ map benchAlgorithm algorithms
  where
    benchAlgorithm :: (String, [Int] -> [Int]) -> Benchmark
    benchAlgorithm (name, sorter) = env testEnv $ bench name . nf sorter

    algorithms :: Ord a => [(String, [a] -> [a])]
    algorithms = [ ("quicksort", quicksort)
                 , ("heapsort",  heapsort)
                 , ("mergesort", mergesort)
                 ]

    testEnv :: IO [Int]
    testEnv = replicateM 1000000 $ randomRIO (1, 10000)

