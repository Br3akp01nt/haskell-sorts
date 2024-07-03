{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Ord.Heapsort (heapsort) where

import Data.Vector.Mutable.Function (withSTVector)
import Control.Monad.Fix (fix)
import Control.Monad.Loops (iterateUntilM, maximumByM)
import qualified Data.Vector.Mutable as VM
import Data.Vector.Mutable (STVector)
import Control.Monad.ST (ST)
import Control.Monad (guard, void, forM_, (>=>), when)
import Data.Maybe (mapMaybe, isJust, catMaybes, fromJust)
import Control.Applicative.Tuple (bothA)
import qualified Data.Vector as V
import Data.Ord.Monad (comparingM)
import Data.Foldable (for_, traverse_)
import Data.Functor (($>))
import Data.Traversable (for)

heapsort :: forall a. (Show a, Ord a) => [a] -> [a]
heapsort = withSTVector heapsortVector

heapsortVector :: forall a s. (Show a, Ord a) => STVector s a -> ST s ()
heapsortVector mVec = do
    heapify
    void $ iterateUntilM (<= 1) popNode (VM.length mVec)
  where
    heapify :: ST s ()
    heapify = forM_ nodes repairHeap
      where
        n = VM.length mVec
        nodes = case parent $ Node (n - 1) n of
                  Nothing -> []
                  Just (Node i _) -> (`Node` n) <$> [i, (i - 1) .. 0]

    popNode :: Int -> ST s Int
    popNode n = do
        VM.swap mVec 0 i
        i <$ repairHeap (Node 0 i)
      where i = n - 1

    repairHeap :: Node -> ST s ()
    repairHeap = siftDown >=> siftUp
      where
        siftDown = iterateUntilM isLeaf swapGreatestChild
        siftUp n = do
            n' <- readNode n
            flip fix n $ \rec x ->
                for_ (parent x) $ \p -> do
                    p' <- readNode p
                    when (p' < n') $ swapNodes p x *> rec p

    swapGreatestChild :: Node -> ST s Node
    swapGreatestChild n = do
        c <- fromJust <$> maxChild n
        c <$ swapNodes n c

    maxChild :: Node -> ST s (Maybe Node)
    maxChild x =
        let children = catMaybes [leftChild x, rightChild x]
         in maximumByM (comparingM readNode) children

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

leftChild :: Node -> Maybe Node
leftChild (Node i n) = Node j n <$ guard (n > j)
  where j = 2 * i + 1

rightChild :: Node -> Maybe Node
rightChild (Node i n) = Node j n <$ guard (n > j)
  where j = 2 * i + 2

isLeaf :: Node -> Bool
isLeaf n = not $ any isJust [leftChild n, rightChild n]

