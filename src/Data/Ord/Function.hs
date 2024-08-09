module Data.Ord.Function where

lessOrEqualOn :: a -> (a -> a -> Ordering) -> a -> Bool
lessOrEqualOn a c b =
    case c a b of
      GT -> False
      _  -> True

greaterOrEqualOn :: a -> (a -> a -> Ordering) -> a -> Bool
greaterOrEqualOn a c b =
    case c a b of
      LT -> False
      _  -> True

