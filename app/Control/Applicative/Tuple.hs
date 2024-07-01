module Control.Applicative.Tuple (bothA, bothA_) where

import Control.Monad (void)
import Data.Composition ((.:))

bothA_ :: Applicative f => (a -> f b) -> (a, a) -> f ()
bothA_ = void .: bothA

bothA :: Applicative f => (a -> f b) -> (a, a) -> f (b, b)
bothA f (a, b) = (,) <$> f a <*> f b

