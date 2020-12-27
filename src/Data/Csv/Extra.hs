{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}

-- | Extensions or replacements for Cassava
module Data.Csv.Extra (
  -- * Streamlined named record parsing
  FromNamedRecord (..),
  CassavaNamedRecord (..),
  NamedRecordParser,
  decodeByName,
  lookup,
) where

import Control.Lens (each, over, _2, _Right)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Csv as Csv
import Data.Vector (Vector)
import Relude
import Prelude hiding (lookup)

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
lookup :: (Csv.FromField a) => BS.ByteString -> NamedRecordParser a
lookup name = do
  namedRecord <- ask
  lift $ Csv.lookup namedRecord name

-- | A replacement for 'Csv.decodeByName'
decodeByName :: (FromNamedRecord a) => C.ByteString -> Either String (Csv.Header, Vector a)
decodeByName csv = over (_Right . _2 . each) (\case CsvFnrWrapper a -> a) $ Csv.decodeByName csv
