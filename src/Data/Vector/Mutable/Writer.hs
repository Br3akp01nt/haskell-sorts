{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module Data.Vector.Mutable.Writer (MVectorWriter(runMVectorWriter), into, tell, tellVector, hoist) where

import           Control.Arrow             (second)
import           Control.Monad             (guard, when, (>=>))
import qualified Control.Monad.Trans.Class as T
import           Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe)
import           Data.Functor              (($>))
import           Data.Vector.Mutable       (MVector, PrimMonad (PrimState),
                                            overlaps)
import qualified Data.Vector.Mutable       as VM

newtype PrimMonad m => MVectorWriter m a b =
  MVectorWriter
    { runMVectorWriter :: MVector (PrimState m) a
                       -> m (Maybe (MVector (PrimState m) a, b))
    }

into :: PrimMonad m => MVectorWriter m a b
                    -> MVector (PrimState m) a
                    -> m (Maybe (MVector (PrimState m) a, b))
into = runMVectorWriter

instance PrimMonad m => Functor (MVectorWriter m a) where
  fmap f w = MVectorWriter $ fmap (fmap $ second f) . into w

instance PrimMonad m => Applicative (MVectorWriter m a) where
  pure x = MVectorWriter $ \v -> pure $ pure (v, x)
  f <*> w = f >>= (<$> w)

instance PrimMonad m => Monad (MVectorWriter m a) where
  w >>= f = MVectorWriter $
      runMVectorWriter w >=> \case
        Nothing -> pure Nothing
        Just (v', w') -> into (f w') v'

tell :: (PrimMonad m, Show a) => a -> MVectorWriter m a ()
tell a = MVectorWriter $ \v -> runMaybeT $ do
      guard $ VM.length v > 0
      VM.write v 0 a
      pure (VM.drop 1 v, ())

tellVector :: PrimMonad m => MVector (PrimState m) a -> MVectorWriter m a ()
tellVector a = MVectorWriter $ \v -> runMaybeT $ do
      let l = VM.length a
      guard $ l <= VM.length v
      guard $ not $ a `overlaps` v
      let (v', r) = VM.splitAt l v
      when (l > 0) $ VM.copy v' a
      pure (r, ())

hoist :: PrimMonad m => m b -> MVectorWriter m a b
hoist m = MVectorWriter $ \v -> fmap (Just . (v,)) m

