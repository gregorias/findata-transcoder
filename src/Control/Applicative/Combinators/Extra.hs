module Control.Applicative.Combinators.Extra (
  some1Till,
  some1Till_,
  surroundedBy,
) where

import Control.Applicative.Combinators (between, manyTill, manyTill_)
import Relude

surroundedBy :: (Applicative f) => f surround -> f a -> f a
surroundedBy surround = between surround surround

-- | A NonEmpty version of 'someTill'
some1Till :: (Alternative f) => f a -> f end -> f (NonEmpty a)
some1Till p end = liftA2 (:|) p (manyTill p end)

-- | A NonEmpty version of 'someTill_'
some1Till_ :: (Alternative f) => f a -> f end -> f (NonEmpty a, end)
some1Till_ p end = liftA2 (\x (xs, y) -> (x :| xs, y)) p (manyTill_ p end)
