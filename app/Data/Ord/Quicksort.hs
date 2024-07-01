module Data.Ord.Quicksort (quicksort) where

import Control.Monad ((<=<), when)
import Control.Monad.ST (ST)
import Data.Vector.Mutable (STVector)
import Control.Monad.Fix (fix)
import Data.STRef (newSTRef, readSTRef, modifySTRef)
import Control.Monad.Loops (untilM_, untilJust)
import Data.Vector.Mutable.Function (withSTVector)
import Control.Applicative.Tuple (bothA_, bothA)
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector as V

quicksort :: Ord a => [a] -> [a]
quicksort = withSTVector $ fix $ \rec xs ->
    when (VM.length xs > 1) $
        partition xs >>= bothA_ rec
  where
    partition :: Ord a => STVector s a -> ST s (STVector s a, STVector s a)
    partition xs = do
        p      <- pivot xs
        (l, h) <- newSTRef `bothA` (-1, VM.length xs)
        untilJust $ do
            increment l `untilM_` (p <=) <$> (xs `at` l)
            decrement h `untilM_` (p >=) <$> (xs `at` h)
            (l', h') <- readSTRef `bothA` (l, h)
            if l' >= h'
               then pure $ Just  $ VM.splitAt l' xs
               else Nothing     <$ VM.swap xs l' h'

    increment = flip modifySTRef (+   1 )
    decrement = flip modifySTRef (+ (-1))
    at     xs = VM.read xs <=< readSTRef
    pivot  xs = VM.read xs $ VM.length xs `div` 2

