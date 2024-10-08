{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad      (forM_, replicateM)
import           Data.Ord.Heapsort  (heapsort)
import           Data.Ord.Mergesort (mergesort)
import           Data.Ord.Quicksort (quicksort)
import           System.Random      (randomRIO)

main :: IO ()
main = do
    xs :: [Int] <- replicateM 15 $ randomRIO (0, 15)
    forM_ algorithms $ \(name, algorithm) -> do
      putStrLn $ "\n" ++ name ++ ":"
      putStrLn $ "Before: " ++ show xs
      putStrLn $ "After:  " ++ show (algorithm xs)
  where
    algorithms =
      [ ("Quicksort" , quicksort)
      , ("Heapsort"  ,  heapsort)
      , ("Mergesort" , mergesort)
      ]

