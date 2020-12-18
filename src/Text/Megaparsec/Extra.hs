module Text.Megaparsec.Extra (butNot, noConsume) where

import Relude
import Text.Megaparsec

-- | @'butNot' this notThat@ succeeds if notThat fails and this succeeds.
-- Only parser this consumes input.
butNot ::
  (MonadParsec e s m) =>
  m a ->
  m b ->
  m a
butNot this notThat = notFollowedBy notThat >> this

-- | A dual to notFollowedBy
-- @'noConsume p'@ succeeds iff parser p succeeds. This parser doesn't consume anything.
noConsume :: (MonadParsec e s m) => m a -> m a
noConsume = try . lookAhead
