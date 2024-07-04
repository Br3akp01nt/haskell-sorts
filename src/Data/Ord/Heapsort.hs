{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Data.Ord.Heapsort (heapsort) where

import           Control.Monad                (guard, void, when, (>=>))
import           Control.Monad.Fix            (fix)
import           Control.Monad.Loops          (iterateUntilM, maximumByM)
import           Control.Monad.ST             (ST)
import           Data.Foldable                (for_, traverse_)
import           Data.Functor                 (($>))
import           Data.Maybe                   (catMaybes, isJust)
import           Data.Ord.Monad               (comparingM)
import qualified Data.Vector                  as V
import           Data.Vector.Mutable          (STVector)
import qualified Data.Vector.Mutable          as VM
import           Data.Vector.Mutable.Function (withSTVector)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Control.Monad.Trans.Class (lift)
import Data.Traversable (for)

heapsort :: forall a. (Show a, Ord a) => [a] -> [a]
heapsort = withSTVector heapsortVector

heapsortVector :: forall a s. (Show a, Ord a) => STVector s a -> ST s ()
heapsortVector mVec = do
    heapify
    void $ iterateUntilM (<= 1) popNode (VM.length mVec)
  where
    heapify :: ST s ()
    heapify = for_ nodes siftDown
      where
        n = VM.length mVec
        nodes = case parent $ Node (n - 1) n of
                  Nothing         -> []
                  Just (Node i _) -> (`Node` n) <$> [i, (i - 1) .. 0]

    popNode :: Int -> ST s Int
    popNode n = do
        VM.swap mVec 0 i
        i <$ siftDown (Node 0 i)
      where i = n - 1

    siftDown :: Node -> ST s ()
    siftDown x = do
        x' <- readNode x
        void $ runMaybeT $ flip fix x $ \rec n -> do
            (mc, v) <- maxChild n
            guard $ v > x' 
            lift $ swapNodes n mc
            rec mc

    maxChild :: Node -> MaybeT (ST s) (Node, a)
    maxChild x = do
        cs <- lift $ for (children x) $ \c -> (c,) <$> readNode c
        guard $ not $ null cs
        pure $ maximumBy (comparing snd) cs

    readNode :: Node -> ST s a
    readNode = VM.read mVec . nodeIndex

    swapNodes :: Node -> Node -> ST s ()
    swapNodes a b = VM.swap mVec (nodeIndex a) (nodeIndex b)

data Node = Node
  { nodeIndex  :: Int
  , nodeLength :: Int
  } deriving Show

parent :: Node -> Maybe Node
parent (Node i n) = Node j n <$ guard (j >= 0)
  where j = (i - 1) `div` 2

children :: Node -> [Node]
children (Node i n) = filter ((< n) . nodeIndex) 
                    $ map (\a -> Node (2 * i + a) n) [1, 2]

