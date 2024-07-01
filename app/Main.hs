{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Ord.Quicksort (quicksort)
import System.Random (randomRIO)
import Control.Monad (replicateM)

main :: IO ()
main = do
  xs :: [Int] <- replicateM 15 $ randomRIO (0, 15)
  putStrLn $ "Before: " ++ show xs
  putStrLn $ "After: "  ++ show (quicksort xs)
        
