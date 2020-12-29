module Text.Megaparsec.Extra (noConsume) where

import Relude
import Text.Megaparsec (MonadParsec (lookAhead, try))

-- | A dual to notFollowedBy
-- @'noConsume p'@ succeeds iff parser p succeeds. This parser doesn't consume
-- anything.
noConsume :: (MonadParsec e s m) => m a -> m a
noConsume = try . lookAhead
