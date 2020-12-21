{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeFamilies #-}

module Hledger.Read.TestUtils
  ( postingParser,
    transactionParser,
    parseTransactionUnsafe,
  )
where

import qualified Control.Lens as L
import Control.Monad (liftM2)
import Control.Monad.Combinators
  ( manyTill,
    someTill,
  )
import Data.Maybe (fromJust)
import Data.Text (pack)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Hledger.Data.Amount (num)
import Hledger.Data.Extra
  ( makeCommodityAmount,
    makeCurrencyAmount,
  )
import Hledger.Data.Lens
  ( pBalanceAssertion,
    pMaybeAmount,
    tDescription,
    tStatus,
  )
import Hledger.Data.Posting (balassert, nullposting)
import qualified Hledger.Data.Transaction as Tr
import Hledger.Data.Types
  ( Amount (..),
    BalanceAssertion (..),
    Posting (..),
    Status (..),
    Transaction,
  )
import Hledupt.Data (decimalParser)
import Relude
import Text.Megaparsec
  ( MonadParsec,
    Token,
    anySingle,
    choice,
    single,
    try,
  )
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (alphaNumChar, char, newline, printChar)
import qualified Text.Megaparsec.Char as Char
import Text.Megaparsec.Char.Extra (eolOrEof, space)
import Text.Megaparsec.Extra (noConsume)

doubleSpace :: (MonadParsec e s m, Token s ~ Char) => m [Char]
doubleSpace = MP.count 2 space

commoditySymbol :: (MonadParsec e s m, Token s ~ Char) => m String
commoditySymbol = liftM2 (:) Char.letterChar (many alphaNumChar)

isCurrency :: String -> Bool
isCurrency = flip elem ["CHF", "USD", "PLN", "EUR"]

commodity :: MP.Parsec Void String Amount
commodity = do
  (maybeSymbol, amount) <-
    try
      ( do
          symbol <- commoditySymbol
          void $ some space
          amount <- decimalParser
          return (Just symbol, amount)
      )
      <|> ( do
              amount <- decimalParser
              symbol <- optional (some space >> commoditySymbol)
              return (symbol, amount)
          )
  void $ many space
  return $
    case maybeSymbol of
      Just symbol ->
        if isCurrency symbol
          then makeCurrencyAmount symbol amount
          else makeCommodityAmount symbol amount
      Nothing -> num amount

accountParser :: MP.Parsec Void String String
accountParser =
  someTill
    printChar
    ( try (doubleSpace >> many space)
        <|> try (many space >> noConsume eolOrEof)
    )

balanceAssertion :: MP.Parsec Void String BalanceAssertion
balanceAssertion = do
  void $ MP.single '=' >> some space
  fmap (fromJust . balassert) commodity <* many space

statusParser :: (MonadParsec e s m, Token s ~ Char) => m Status
statusParser =
  choice
    [ try (single '*') $> Cleared,
      try (single '!') $> Pending,
      pure Unmarked
    ]

-- | A partial Posting parser
postingParser :: MP.Parsec Void String Posting
postingParser = do
  status <- many space *> statusParser <* many space
  account <- accountParser
  amount <- optional (try commodity)
  let amountSetter = maybe id (L.set pMaybeAmount . Just) amount
  balAssert <- optional (try balanceAssertion)
  void eolOrEof
  return $
    nullposting {paccount = pack account, pstatus = status}
      & amountSetter
        . L.set pBalanceAssertion balAssert

-- | A partial Transaction parser
--
-- This parser parses typical Transaction syntax.
-- It does not conform to the full Ledger spec.
transactionParser :: MP.Parsec Void String Transaction
transactionParser = do
  date <-
    manyTill MP.anySingle (some $ char ' ')
      >>= parseTimeM True defaultTimeLocale "%Y/%m/%d"
  status <- statusParser <* many space
  title <- manyTill anySingle (try newline)
  ps <- some postingParser
  return $
    Tr.transaction date ps
      & L.set tStatus status
        . L.set tDescription title

parseTransactionUnsafe :: String -> Transaction
parseTransactionUnsafe = fromJust . MP.parseMaybe transactionParser
