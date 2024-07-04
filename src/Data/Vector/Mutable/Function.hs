{-# LANGUAGE RankNTypes #-}

module Data.Vector.Mutable.Function (withSTVector) where

import           Control.Monad.ST    (ST, runST)
import qualified Data.Vector         as V
import           Data.Vector.Mutable (STVector)

withSTVector :: (forall s. STVector s a -> ST s ()) -> [a] -> [a]
withSTVector f xs = runST $ do
    mVec <- V.thaw $ V.fromList xs
    f mVec
    V.toList <$> V.freeze mVec

