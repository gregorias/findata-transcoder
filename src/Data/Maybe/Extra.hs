module Data.Maybe.Extra (
  ToMaybeX (..),
) where

import Relude

-- | A class for implicitly converting a value to a 'Maybe' value.
class ToMaybeX x a where
  toMaybeX :: a -> Maybe x

instance (a ~ x) => ToMaybeX x a where
  toMaybeX = pure

instance {-# OVERLAPPING #-} (a ~ x) => ToMaybeX x (Maybe a) where
  toMaybeX = id
