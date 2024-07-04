{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Data.Ord.Heapsort (heapsort) where

import Control.Monad.Fix (fix)
import Control.Monad.Loops (iterateUntilM, maximumByM)
import Control.Monad.ST (ST)
import Control.Monad (guard, void, (>=>), when)
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Maybe (isJust, catMaybes)
import Data.Ord.Monad (comparingM)
import Data.Vector.Mutable (STVector)
import Data.Vector.Mutable.Function (withSTVector)
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector as V

heapsort :: forall a. (Show a, Ord a) => [a] -> [a]
heapsort = withSTVector heapsortVector

heapsortVector :: forall a s. (Show a, Ord a) => STVector s a -> ST s ()
heapsortVector mVec = do
    heapify
    void $ iterateUntilM (<= 1) popNode (VM.length mVec)
  where
    heapify :: ST s ()
    heapify = for_ nodes repairHeap
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
    swapGreatestChild n = maxChild n >>= \case
      Nothing -> pure n
      Just  c -> swapNodes n c $> c

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

