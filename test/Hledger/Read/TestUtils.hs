{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Hledger.Read.TestUtils (
  postingP,
  transactionP,
  parseTransactionUnsafe,
) where

import qualified Control.Lens as L
import Control.Monad (liftM2)
import Control.Monad.Combinators (
  manyTill,
  someTill,
 )
import Data.Maybe (fromJust)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Hledger (
  AmountPrice (TotalPrice, UnitPrice),
  amountSetFullPrecision,
  missingamt,
  post,
 )
import Hledger.Data.Amount (num)
import Hledger.Data.Extra (
  makeCommodityAmount,
  setCurrencyPrecision,
 )
import Hledger.Data.Lens (
  pBalanceAssertion,
  pStatus,
  tDescription,
  tStatus,
 )
import Hledger.Data.Posting (balassert)
import qualified Hledger.Data.Transaction as Tr
import Hledger.Data.Types (
  Amount (..),
  BalanceAssertion (..),
  Posting (..),
  Status (..),
  Transaction,
 )
import Hledupt.Data.MyDecimal (decimalP, defaultDecimalFormat)
import Relude
import Text.Megaparsec (
  MonadParsec (lookAhead),
  Parsec,
  Token,
  anySingle,
  choice,
  single,
  try,
 )
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (
  alphaNumChar,
  char,
  newline,
  printChar,
  spaceChar,
  string,
 )
import qualified Text.Megaparsec.Char as Char
import Text.Megaparsec.Char.Extra (eolOrEof)

type Parser = Parsec Void Text

space :: (MonadParsec e s m, Token s ~ Char) => m Char
space = single ' '

doubleSpace :: (MonadParsec e s m, Token s ~ Char) => m [Char]
doubleSpace = MP.count 2 space

commoditySymbol :: (MonadParsec e s m, Token s ~ Char) => m String
commoditySymbol = liftM2 (:) Char.letterChar (many alphaNumChar)

isCurrency :: Text -> Bool
isCurrency = flip elem ["CHF", "USD", "PLN", "EUR"]

commodityP :: Parser Amount
commodityP = do
  (maybeSymbol, amount) <-
    try
      ( do
          symbol <- commoditySymbol
          void $ some space
          amount <- decimalP defaultDecimalFormat
          return (Just symbol, amount)
      )
      <|> ( do
              amount <- decimalP defaultDecimalFormat
              symbol <- optional (many space >> commoditySymbol)
              return (symbol, amount)
          )
  void $ many space
  return $
    case maybeSymbol of
      Just symbol -> makeCommodityAmount (toText symbol) amount
      Nothing -> num amount

whenCurrencyAdjustStyle :: Amount -> Amount
whenCurrencyAdjustStyle amt
  | isCurrency (acommodity amt) = setCurrencyPrecision amt
  | otherwise = amt

accountParser :: Parser Text
accountParser =
  toText
    <$> someTill
      printChar
      ( try (void $ doubleSpace >> many space)
          <|> try (void $ optional space >> lookAhead eolOrEof)
      )

balanceAssertion :: Parser BalanceAssertion
balanceAssertion = do
  void $ MP.single '=' >> some space
  fmap (fromJust . balassert . whenCurrencyAdjustStyle) commodityP <* many space

statusParser :: (MonadParsec e s m, Token s ~ Char) => m Status
statusParser =
  choice
    [ try (single '*') $> Cleared
    , try (single '!') $> Pending
    , pure Unmarked
    ]

amountPriceParser :: Parser AmountPrice
amountPriceParser = do
  constructor <-
    choice
      [ try (string "@@") $> TotalPrice
      , single '@' $> UnitPrice
      ]
  void $ some spaceChar
  constructor . amountSetFullPrecision <$> commodityP

-- | A partial Posting parser
postingP :: Parser Posting
postingP = do
  status <- many space *> statusParser <* many space
  account <- accountParser
  amount <- whenCurrencyAdjustStyle <$> (try commodityP <|> pure missingamt)
  balAssert <- optional (try balanceAssertion)
  void $ many $ single ' '
  amountPrice <- optional amountPriceParser
  let amount' =
        amount
          { aprice = amountPrice
          }
  void eolOrEof
  return $
    post account amount'
      & L.set pBalanceAssertion balAssert
        . L.set pStatus status

-- | A partial Transaction parser
--
-- This parser parses typical Transaction syntax.
-- It does not conform to the full Ledger spec.
transactionP :: Parser Transaction
transactionP = do
  date <-
    manyTill MP.anySingle (some $ char ' ')
      >>= parseTimeM True defaultTimeLocale "%Y/%m/%d"
  status <- statusParser <* many space
  title <- manyTill anySingle (try newline)
  ps <- some postingP
  return $
    Tr.transaction date ps
      & L.set tStatus status
        . L.set tDescription title

parseTransactionUnsafe :: Text -> Transaction
parseTransactionUnsafe = fromJust . MP.parseMaybe transactionP
