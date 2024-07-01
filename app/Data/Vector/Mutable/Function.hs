{-# LANGUAGE RankNTypes #-}

module Data.Vector.Mutable.Function (withSTVector) where

import Data.Vector.Mutable (STVector)
import qualified Data.Vector as V
import Control.Monad.ST (ST, runST)

withSTVector :: (forall s. STVector s a -> ST s ()) -> [a] -> [a]
withSTVector f xs = runST $ do
    mVec <- V.thaw $ V.fromList xs
    f mVec
    V.toList <$> V.freeze mVec

