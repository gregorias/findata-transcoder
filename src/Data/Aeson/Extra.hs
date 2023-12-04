module Data.Aeson.Extra (
  eitherDecodeStrict,
) where

import Data.Aeson qualified as Aeson
import Data.Either.Extra (mapLeft)
import Data.Text qualified as T
import Relude

-- | Like 'Aeson.eitherDecodeStrict' but with a 'Text' error message.
eitherDecodeStrict :: (Aeson.FromJSON a) => ByteString -> Either Text a
eitherDecodeStrict = Aeson.eitherDecodeStrict >>> mapLeft T.pack
