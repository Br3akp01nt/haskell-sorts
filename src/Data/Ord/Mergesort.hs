module Data.Ord.Mergesort (mergesort) where

import Data.Function (on)

mergesort :: Ord a => [a] -> [a]
mergesort []  = []
mergesort [x] = [x]
mergesort xs  = (merge `on` mergesort) left right
  where (left, right) = splitAt (length xs `div` 2) xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
 | x < y = x : merge xs (y:ys)
 | otherwise = y : merge (x:xs) ys

