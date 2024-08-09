{-# LANGUAGE ScopedTypeVariables #-}

module Data.Ord.Quicksort (quicksort) where

import           Control.Applicative.Tuple    (bothA, bothA_)
import           Control.Monad                (when, (<=<))
import           Control.Monad.Fix            (fix)
import           Control.Monad.Loops          (untilJust, untilM_)
import           Control.Monad.ST             (ST)
import           Data.Ord.Function            (greaterOrEqualOn, lessOrEqualOn)
import           Data.STRef                   (modifySTRef, newSTRef, readSTRef)
import qualified Data.Vector                  as V
import           Data.Vector.Mutable          (STVector)
import qualified Data.Vector.Mutable          as VM
import           Data.Vector.Mutable.Function (withSTVector)

quicksort :: Ord a => [a] -> [a]
quicksort = quicksortBy compare

quicksortBy :: forall a. (a -> a -> Ordering) -> [a] -> [a]
quicksortBy c = withSTVector $ fix $ \rec xs ->
    when (VM.length xs > 1) $
        partition xs >>= bothA_ rec
  where
    partition :: STVector s a -> ST s (STVector s a, STVector s a)
    partition xs = do
        p      <- pivot xs
        (l, h) <- newSTRef `bothA` (-1, VM.length xs)
        untilJust $ do
            increment l `untilM_` (p  `lessOrEqualOn`   c) <$> (xs `at` l)
            decrement h `untilM_` (p `greaterOrEqualOn` c) <$> (xs `at` h)
            (l', h') <- readSTRef `bothA` (l, h)
            if l' >= h'
               then pure $ Just  $ VM.splitAt l' xs
               else Nothing     <$ VM.swap xs l' h'

    increment = (`modifySTRef` (+   1 ))
    decrement = (`modifySTRef` (+ (-1)))
    at     xs = VM.read xs <=< readSTRef
    pivot  xs = VM.read xs $ VM.length xs `div` 2

