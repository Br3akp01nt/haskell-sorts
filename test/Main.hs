{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Control.Monad        (void)
import           Data.List            (intercalate)
import           Data.Ord.Heapsort    (heapsort)
import           Data.Ord.Mergesort   (mergesort)
import           Data.Ord.Quicksort   (quicksort)
import           System.Exit          (exitFailure, exitSuccess)
import           System.IO            (BufferMode (NoBuffering), hSetBuffering,
                                       stdout)
import           Test.QuickCheck      (Arbitrary (arbitrary), Property,
                                       Testable (..), allProperties, collect,
                                       conjoin, counterexample, elements,
                                       isSuccess, label, quickCheckAll,
                                       quickCheckResult, verbose, (===))
import           Test.QuickCheck.Test (quickCheck)

data SortingAlgorithm a =
  SortingAlgorithm
    { algorithmName :: String
    , algorithm     :: [a] -> [a]
    }

algorithms :: (Show a, Ord a) => [SortingAlgorithm a]
algorithms = map (uncurry SortingAlgorithm)
                 [ ("Quicksort"  ,  quicksort)
                 , ("Heapsort"   ,   heapsort)
                 , ("Mergesort"  ,  mergesort)
                 ]

prop_sortAlreadySortedIsId :: [Int] -> SortingAlgorithm Int -> Property
prop_sortAlreadySortedIsId xs (SortingAlgorithm algName alg) = alg xs === alg (alg xs)

prop_actuallySorts :: [Int] -> SortingAlgorithm Int -> Property
prop_actuallySorts xs (SortingAlgorithm algName alg) =
  counterexample (show $ alg xs) $ isSorted $ alg xs

isSorted :: Ord a => [a] -> Bool
isSorted (x:y:xs) = x <= y && isSorted (y:xs)
isSorted _        = True

instance (Ord a, Show a) => Arbitrary (SortingAlgorithm a) where
  arbitrary = elements algorithms

instance Show (SortingAlgorithm a) where
  show = algorithmName

return []

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  pass <- $quickCheckAll
  if pass then exitSuccess
          else exitFailure

