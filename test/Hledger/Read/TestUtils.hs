{-# LANGUAGE TemplateHaskellQuotes #-}

module Hledger.Read.TestUtils (
  postingP,
  transactionP,
  parseTransactionUnsafe,
  transactionQQ,
  transactionsQQ,
) where

import qualified Control.Lens as L
import Control.Monad.Combinators (
  manyTill,
  someTill,
 )
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Hledger (
  AmountPrice (..),
  amountSetFullPrecision,
  missingamt,
  post,
 )
import Hledger.Data.Amount (num)
import Hledger.Data.Extra (
  makeCommodityAmount,
  setCurrencyPrecision,
 )
import Hledger.Data.Lens (pBalanceAssertion, pComment, pStatus, tDescription, tStatus)
import Hledger.Data.Posting (balassert)
import qualified Hledger.Data.Transaction as Tr
import Hledger.Data.Types (
  Amount (..),
  BalanceAssertion (..),
  Posting (..),
  Status (..),
  Transaction,
 )
import qualified Hledger.Read.Common as Hledger (
  descriptionp,
  statusp,
 )
import qualified Hledger.Utils.Parse as Hledger
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
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (
  char,
  newline,
  printChar,
  spaceChar,
  string,
 )
import qualified Text.Megaparsec.Char as MP
import Text.Megaparsec.Char.Extra (eolOrEof)
import qualified Text.Megaparsec.Internal as MP
import Transcoder.Data.MyDecimal (decimalP, defaultDecimalFormat)
import Trimdent (trimdent)

type Parser = Parsec () Text

toParser :: Hledger.TextParser Identity a -> Parser a
toParser = MP.withParsecT (const ())

space :: (MonadParsec e s m, Token s ~ Char) => m Char
space = single ' '

doubleSpace :: (MonadParsec e s m, Token s ~ Char) => m [Char]
doubleSpace = MP.count 2 space

isCurrency :: Text -> Bool
isCurrency = flip elem ["CHF", "USD", "PLN", "EUR"]

commoditySymbolP :: Parser Text
commoditySymbolP =
  label "commodity symbol" $
    quotedCommoditySymbolP <|> simpleCommoditySymbolP

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
  return $
    case maybeSymbol of
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

amountPriceParser :: Parser AmountPrice
amountPriceParser = do
  constructor <-
    choice
      [ try (string "@@") $> TotalPrice
      , single '@' $> UnitPrice
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
  amountPrice <- optional amountPriceParser
  void $ many $ single ' '
  comment <- T.strip . fromMaybe "" <$> optional commentP
  let amount' =
        amount
          { aprice = amountPrice
          }
  void eolOrEof
  return $
    post account amount'
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
  return $
    Tr.transaction date ps
      & L.set tStatus status
        . L.set tDescription title

transactionsP :: Parser [Transaction]
transactionsP = MP.many transactionP

parseTransactionUnsafe :: Text -> Transaction
parseTransactionUnsafe = fromJust . MP.parseMaybe transactionP

parseTransactionsUnsafe :: Text -> [Transaction]
parseTransactionsUnsafe = fromJust . MP.parseMaybe transactionsP

transactionQQExp :: String -> Q Exp
transactionQQExp trString = [|parseTransactionUnsafe input|]
 where
  input = toText . trimdent $ trString

transactionsQQExp :: String -> Q Exp
transactionsQQExp trString = [|parseTransactionsUnsafe input|]
 where
  input = toText . trimdent $ trString

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
