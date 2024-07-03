module Data.Ord.Monad (comparingM) where

comparingM :: (Monad m, Ord a) => (b -> m a) -> b -> b -> m Ordering
comparingM f x y = compare <$> f x <*> f y

