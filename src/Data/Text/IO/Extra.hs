module Data.Text.IO.Extra (readFileUtf8) where

import Data.ByteString (readFile)
import Relude

-- Reads a file encoded with UTF-8.
readFileUtf8 :: MonadIO m => FilePath -> m Text
readFileUtf8 = liftIO . fmap decodeUtf8 . Data.ByteString.readFile
