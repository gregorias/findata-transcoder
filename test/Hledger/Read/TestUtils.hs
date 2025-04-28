{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hledger.Read.TestUtils (
  postingP,
  transactionP,
  transactionQQ,
  transactionsQQ,
) where

import Control.Lens qualified as L
import Control.Monad.Combinators (
  manyTill,
  someTill,
 )
import Data.Char (isDigit)
import Data.Decimal.Extra (decimalP, defaultDecimalFormat)
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Hledger (
  AmountCost (..),
  amountSetFullPrecision,
  missingamt,
  post,
 )
import Hledger qualified as H
import Hledger.Data.Amount (num)
import Hledger.Data.Extra (
  makeCommodityAmount,
  setCurrencyPrecision,
 )
import Hledger.Data.Lens (pBalanceAssertion, pComment, pStatus, tDescription, tStatus)
import Hledger.Data.Posting (balassert)
import Hledger.Data.Transaction qualified as Tr
import Hledger.Data.Types (
  Amount (..),
  BalanceAssertion (..),
  Posting (..),
  Status (..),
  Transaction,
 )
import Hledger.Extra ()
import Hledger.Read.Common qualified as Hledger (
  descriptionp,
  statusp,
 )
import Hledger.Utils.Parse qualified as Hledger
import Language.Haskell.TH.Quote (
  QuasiQuoter (..),
 )
import Language.Haskell.TH.Syntax (
  Exp (..),
  Q (..),
 )
import Relude
import Text.Megaparsec (
  MonadParsec (lookAhead),
  Parsec,
  Token,
  anySingle,
  between,
  choice,
  label,
  single,
  takeWhile1P,
  try,
 )
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char (
  char,
  newline,
  printChar,
  spaceChar,
  string,
 )
import Text.Megaparsec.Char qualified as MP
import Text.Megaparsec.Char.Extra (eolOrEof)
import Text.Megaparsec.Extra as MP
import Text.Megaparsec.Internal qualified as MP
import Trimdent (trimdent)

type Parser = Parsec H.HledgerParseErrorData Text

toParser :: Hledger.TextParser Identity a -> Parser a
toParser = MP.withParsecT id

space :: (MonadParsec e s m, Token s ~ Char) => m Char
space = single ' '

doubleSpace :: (MonadParsec e s m, Token s ~ Char) => m [Char]
doubleSpace = MP.count 2 space

isCurrency :: Text -> Bool
isCurrency = flip elem ["CHF", "USD", "PLN", "EUR"]

commoditySymbolP :: Parser Text
commoditySymbolP =
  label "commodity symbol"
    $ quotedCommoditySymbolP
    <|> simpleCommoditySymbolP

quotedCommoditySymbolP :: Parser Text
quotedCommoditySymbolP =
  between (char '"') (char '"') $ takeWhile1P Nothing f
 where
  f c = c /= ';' && c /= '\n' && c /= '\"'

simpleCommoditySymbolP :: Parser Text
simpleCommoditySymbolP = takeWhile1P Nothing (not . isNonsimpleCommodityChar)

-- characters that may not be used in a non-quoted commodity symbol
isNonsimpleCommodityChar :: Char -> Bool
isNonsimpleCommodityChar = liftA2 (||) isDigit isOther
 where
  otherChars :: String = "-+.@*;\t\n \"{}="
  isOther c = c `elem` otherChars

commodityP :: Parser Amount
commodityP = do
  (maybeSymbol, amount) <-
    try
      ( do
          symbol <- commoditySymbolP
          void $ some space
          amount <- decimalP defaultDecimalFormat
          return (Just symbol, amount)
      )
      <|> ( do
              amount <- decimalP defaultDecimalFormat
              symbol <- optional (many space >> commoditySymbolP)
              return (symbol, amount)
          )
  void $ many space
  return
    $ case maybeSymbol of
      Just symbol -> makeCommodityAmount symbol amount
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

statusP :: Parser Status
statusP = toParser Hledger.statusp

descriptionP :: Parser Text
descriptionP = toParser Hledger.descriptionp

amountCostParser :: Parser AmountCost
amountCostParser = do
  constructor <-
    choice
      [ try (string "@@") $> TotalCost
      , single '@' $> UnitCost
      ]
  void $ some spaceChar
  constructor . amountSetFullPrecision <$> commodityP

commentP :: Parser Text
commentP = do
  void $ single ';'
  toText <$> manyTill anySingle (lookAhead eolOrEof)

-- | A partial Posting parser
postingP :: Parser Posting
postingP = do
  status <-
    ( do
        status <- statusP
        case status of
          Unmarked -> MP.hspace1
          Pending -> MP.hspace
          Cleared -> MP.hspace
        return status
    )
  account <- accountParser
  amount <- whenCurrencyAdjustStyle <$> (try commodityP <|> pure missingamt)
  balAssert <- optional (try balanceAssertion)
  void $ many $ single ' '
  amountCost <- optional amountCostParser
  void $ many $ single ' '
  comment <- T.strip . fromMaybe "" <$> optional commentP
  let amount' =
        amount
          { acost = amountCost
          }
  void eolOrEof
  return
    $ post account amount'
    & L.set pBalanceAssertion balAssert
    . L.set pStatus status
    . L.set pComment comment

-- | A partial Transaction parser
--
-- This parser parses typical Transaction syntax.
-- It does not conform to the full Ledger spec.
transactionP :: Parser Transaction
transactionP = do
  date <-
    manyTill MP.anySingle (some $ char ' ')
      >>= parseTimeM True defaultTimeLocale "%Y/%m/%d"
  status <- statusP <* many space
  title <- descriptionP <* newline
  ps <- some postingP
  return
    $ Tr.transaction date ps
    & L.set tStatus status
    . L.set tDescription title

transactionsP :: Parser [Transaction]
transactionsP = MP.many transactionP

transactionQQExp :: String -> Q Exp
transactionQQExp trString = case eitherTr of
  Right tr -> [|tr|]
  Left err -> fail $ show err
 where
  input = toText . trimdent $ trString
  eitherTr = MP.parsePretty transactionP "" input

transactionsQQExp :: String -> Q Exp
transactionsQQExp trString = case eitherTrs of
  Right trs -> [|trs|]
  Left err -> fail $ show err
 where
  input = toText . trimdent $ trString
  eitherTrs = MP.parsePretty transactionsP "" input

transactionQQ :: QuasiQuoter
transactionQQ =
  QuasiQuoter
    { quoteExp = transactionQQExp
    , quotePat = error "quotePat is unimplemented"
    , quoteType = error "quoteType is unimplemented"
    , quoteDec = error "quoteDec is unimplemented"
    }

transactionsQQ :: QuasiQuoter
transactionsQQ =
  QuasiQuoter
    { quoteExp = transactionsQQExp
    , quotePat = error "quotePat is unimplemented"
    , quoteType = error "quoteType is unimplemented"
    , quoteDec = error "quoteDec is unimplemented"
    }
