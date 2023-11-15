{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

-- | Extensions or replacements for Cassava
module Data.Csv.Extra (
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
import Data.Csv qualified as Csv
import Data.Vector (Vector)
import Relude
import Text.Megaparsec (MonadParsec)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MP
import Prelude hiding (elem, lookup)

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
