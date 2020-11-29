{-# LANGUAGE DuplicateRecordFields #-}

-- | Bcge.Hint is a mechanism that generates from BCGE transaction titles
-- - additional data that you can use to generate more informational Ledger
-- - transactions.
-- -
-- - You can fetch config for generating hints from a CSV file to have that
-- - config in a private file instead of on Github.
module Hledupt.Bcge.Hint
  ( TransactionHint (..),
    Config,
    ConfigEntry (..),
    configParser,
    transactionTitleToHint,
  )
where

import Control.Monad (void)
import Data.List (isInfixOf)
import Data.Maybe (mapMaybe)
import Safe (headMay)
import Text.Megaparsec
  ( Parsec,
    anySingle,
    eof,
    many,
    noneOf,
    notFollowedBy,
    some,
    (<|>),
  )
import Text.Megaparsec.Char (char, eol, string)

data TransactionHint = TransactionHint
  { title :: String,
    counterAccount :: String
  }
  deriving (Eq, Show)

data ConfigEntry = ConfigEntry
  { keyword :: String,
    hint :: TransactionHint
  }
  deriving (Eq, Show)

type Config = [ConfigEntry]

checkTransactionTitleToHint :: String -> ConfigEntry -> Maybe TransactionHint
checkTransactionTitleToHint titleArg (ConfigEntry keywordArg hintArg)
  | keywordArg `isInfixOf` titleArg = Just hintArg
  | otherwise = Nothing

transactionTitleToHint :: Config -> String -> Maybe TransactionHint
transactionTitleToHint config titleArg =
  headMay . mapMaybe (checkTransactionTitleToHint titleArg) $ config

-- Config parser

type StringParser = Parsec () String

-- TODO generalize
eolOrEof :: StringParser ()
eolOrEof = void eol <|> eof

headerParser :: StringParser String
headerParser = string "keyword,title,counterAccount" <* eolOrEof

configEntryParser :: StringParser ConfigEntry
configEntryParser = do
  keywordValue <- some (noneOf ",") <* char ','
  titleValue <- some (noneOf ",") <* char ','
  counterAccountValue <- some $ notFollowedBy eolOrEof *> anySingle
  eolOrEof
  return $ ConfigEntry keywordValue (TransactionHint titleValue counterAccountValue)

configParser :: StringParser Config
configParser = do
  _ <- headerParser
  many configEntryParser
