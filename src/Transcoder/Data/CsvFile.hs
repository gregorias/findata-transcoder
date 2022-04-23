{-# LANGUAGE DeriveFunctor #-}

-- | A wrapper type to indicate that a string
-- is a CSV file.
module Transcoder.Data.CsvFile (
  CsvFile (..),
) where

import Relude

newtype CsvFile a = CsvFile
  { unCsvFile :: a
  }
  deriving newtype
    ( IsString
    , ToString
    , Show
    , Eq
    )
  deriving stock (Functor)
