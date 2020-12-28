-- | A wrapper type to indicate that a string
-- is a CSV file.
module Hledupt.Data.CsvFile (
  CsvFile (..),
) where

import Relude

newtype CsvFile a = CsvFile
  { unCsv :: a
  }
  deriving newtype
    ( IsString
    , ToString
    , Show
    , Eq
    )
