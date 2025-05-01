{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

-- | Extensions or replacements for Cassava
module Data.Csv.Extra (
  -- * Named record utilities
  showNamedRecord,

  -- * Parsing utilities
  prependContextOnFailure,

  -- * Streamlined named record parsing
  FromNamedRecord (..),
  CassavaNamedRecord (..),
  NamedRecordParser,
  decodeByName,
  lookup,

  -- * Megaparsec-compatible parsers
  fieldP,
  quotedFieldP,
) where

import Control.Applicative.Combinators.Extra (surroundedBy)
import Control.Lens (each, over, _2, _Right)
import Data.ByteString qualified as BS
import Data.Csv qualified as Csv
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Vector (Vector)
import Relude
import Text.Megaparsec (MonadParsec)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MP
import Prelude hiding (elem, lookup)

-- | Shows a named record in a human-readable format.
--
-- This is meant for debugging and logging. I don't care much about ambiguity.
showNamedRecord :: Csv.NamedRecord -> Text
showNamedRecord nr =
  T.concat
    [ "{"
    , T.intercalate "," (map (\(k, v) -> decodeUtf8 k <> ":" <> decodeUtf8 v) items)
    , "}"
    ]
 where
  items :: [(BS.ByteString, BS.ByteString)]
  items = HM.toList nr

-- | Prepends context to a 'Csv.Parser' error message.
prependContextOnFailure :: String -> Csv.Parser a -> Csv.Parser a
prependContextOnFailure context parser = do
  -- Csv.Parser is an opaque type, so there's no other way to prepend than to
  -- use 'runParser' to deconstruct it.
  case Csv.runParser parser of
    Left err -> fail $ context <> err
    Right val -> pure val

type NamedRecordParser = ReaderT Csv.NamedRecord Csv.Parser

-- | A replacement for 'Data.Csv.FromNamedRecord' that uses a
-- 'NamedRecordParser' to avoid lookup boilerplate.
class FromNamedRecord a where
  parseNamedRecord :: NamedRecordParser a

newtype CassavaNamedRecord a = CassavaNamedRecord
  { unCassavaNamedRecord :: a
  }

instance
  (Csv.FromNamedRecord a) =>
  FromNamedRecord (CassavaNamedRecord a)
  where
  parseNamedRecord = ask >>= (lift . fmap CassavaNamedRecord . Csv.parseNamedRecord)

newtype CsvFnrWrapper a = CsvFnrWrapper a

instance (FromNamedRecord a) => Csv.FromNamedRecord (CsvFnrWrapper a) where
  parseNamedRecord = fmap CsvFnrWrapper . runReaderT parseNamedRecord

-- | A replacement for 'Csv.lookup'
lookup :: (Csv.FromField a) => ByteString -> NamedRecordParser a
lookup name = do
  namedRecord <- ask
  lift $ Csv.lookup namedRecord name

-- | A replacement for 'Csv.decodeByName'
decodeByName :: (FromNamedRecord a) => LByteString -> Either String (Csv.Header, Vector a)
decodeByName csv =
  over
    (_Right . _2 . each)
    (\case CsvFnrWrapper a -> a)
    $ Csv.decodeByName csv

-- | RFC 4180-compatible CSV quoted field parser that decodes the field.
quotedFieldP :: (MonadParsec e s m, MP.Token s ~ Char, MP.Tokens s ~ Text) => m Text
quotedFieldP = MP.label "quoted CSV field" . surroundedBy (MP.char '"') $ do
  -- takeWhile1P is important, so that the internal parser accepts something,
  -- otherwise we have an infinite loop.
  chunks <- MP.many (MP.takeWhile1P Nothing (/= '"') <|> (MP.string "\"\"" $> "\""))
  return $ mconcat chunks

-- | RFC 4180-compatible CSV field parser.
fieldP :: (MonadParsec e s m, MP.Token s ~ Char, MP.Tokens s ~ Text) => m Text
fieldP = MP.label "CSV field" $ quotedFieldP <|> unquotedField
 where
  -- Potentially essential to ensure we've actually read the entire field, and
  -- fieldEnding = void eolOrEof <|> void (MP.char ',')

  unquotedField =
    MP.label "unquoted CSV field" $ MP.takeWhileP Nothing (not . (`elem` [',', '\r', '\n', '"']))
