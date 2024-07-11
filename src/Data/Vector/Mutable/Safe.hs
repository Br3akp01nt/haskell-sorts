module Data.Vector.Mutable.Safe (head) where

import           Control.Monad.ST    (ST)
import           Data.Vector.Mutable (STVector)
import qualified Data.Vector.Mutable as VM
import           Prelude             hiding (head)

head :: STVector s a -> ST s (Maybe a)
head = (`VM.readMaybe` 0)

