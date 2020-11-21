-- | Bcge.Hint is a mechanism that generates from BCGE transaction titles
-- - additional data that you can use to generate more informational Ledger
-- - transactions.
-- -
-- - You can fetch config for generating hints from a CSV file to have that
-- - config in a private file instead of on Github.
module Bcge.Hint
  ( TransactionHint (..),
    Config,
    ConfigEntry (..),
    configParser,
    transactionTitleToHint,
  )
where

import Control.Monad (void)
import Data.Function ((&))
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
    oneOf,
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
checkTransactionTitleToHint title (ConfigEntry keyword hint)
  | keyword `isInfixOf` title = Just hint
  | otherwise = Nothing

transactionTitleToHint :: Config -> String -> Maybe TransactionHint
transactionTitleToHint config title =
  headMay . mapMaybe (checkTransactionTitleToHint title) $ config

-- Config parser

type StringParser = Parsec () String

eolOrEof :: StringParser ()
eolOrEof = void eol <|> eof

headerParser :: StringParser String
headerParser = string "keyword,title,counterAccount" <* eolOrEof

configEntryParser :: StringParser ConfigEntry
configEntryParser = do
  keyword <- some (noneOf ",") <* char ','
  title <- some (noneOf ",") <* char ','
  counterAccount <- some $ notFollowedBy eolOrEof *> anySingle
  eolOrEof
  return $ ConfigEntry keyword (TransactionHint title counterAccount)

configParser :: StringParser Config
configParser = do
  headerParser
  many configEntryParser
