{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Data.Ord.Mergesort (mergesort) where

import           Control.Applicative.Tuple    (bothA, bothA_)
import           Control.Arrow                (Arrow (second))
import           Control.Monad                (guard, void, (>=>))
import           Control.Monad.Fix            (fix)
import           Control.Monad.ST             (ST)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Maybe    (runMaybeT)
import           Data.Function                (on)
import           Data.Functor                 (($>))
import           Data.Maybe                   (fromJust)
import           Data.STRef                   (newSTRef)
import qualified Data.Vector                  as V
import           Data.Vector.Mutable          (STVector)
import qualified Data.Vector.Mutable          as VM
import           Data.Vector.Mutable.Function (withSTVector)
import qualified Data.Vector.Mutable.Safe     as VS
import           Data.Vector.Mutable.Writer   (MVectorWriter (..), hoist, into,
                                               tell, tellVector)

mergesort :: (Show a, Ord a) => [a] -> [a]
mergesort = withSTVector $ fix $ \rec mVec -> void $ runMaybeT $ do
    guard $ VM.length mVec > 1
    let (left, right) = VM.splitAt (VM.length mVec `div` 2) mVec
    (leftRes, rightRes) <- lift $ flip bothA (left, right) $ \src -> do
        dst <- VM.new (VM.length src)
        VM.copy dst src
        rec dst $> dst
    void $ lift $ merge leftRes rightRes `into` mVec
  where
    merge :: forall s a. (Ord a, Show a) => STVector s a -> STVector s a -> MVectorWriter (ST s) a ()
    merge xs ys = do
      hoist (bothA VS.head (xs, ys)) >>= \case
        (Nothing, Nothing) -> pure ()
        (Nothing,  Just y) -> tell y *> tellVector (VM.tail ys)
        (Just x , Nothing) -> tell x *> tellVector (VM.tail xs)
        (Just x , Just y ) | x < y     -> tell x *> merge (VM.tail xs) ys
                           | otherwise -> tell y *> merge xs (VM.tail ys)

