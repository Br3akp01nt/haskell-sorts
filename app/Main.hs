module Main where

import Data.Ord.Quicksort (quicksort)

main :: IO ()
main = print $ quicksort xs
  where
    xs :: [Int]
    xs = [8, 3, 5, 2, 5, 7, 23, 562, 34, 63, 1, 18, 1, 2, 52, 23, 12, 1, 2, 1]
