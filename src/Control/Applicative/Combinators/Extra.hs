module Control.Applicative.Combinators.Extra (some1Till, some1Till_) where

import Control.Applicative.Combinators (manyTill, manyTill_)
import Relude (Alternative, NonEmpty ((:|)), liftA2)

-- | A NonEmpty version of 'someTill'
some1Till :: (Alternative f) => f a -> f end -> f (NonEmpty a)
some1Till p end = liftA2 (:|) p (manyTill p end)

-- | A NonEmpty version of 'someTill_'
some1Till_ :: (Alternative f) => f a -> f end -> f (NonEmpty a, end)
some1Till_ p end = liftA2 (\x (xs, y) -> (x :| xs, y)) p (manyTill_ p end)
