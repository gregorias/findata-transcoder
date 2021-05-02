{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Hledupt.BcgeCC (
  rechnungToLedger,
) where

import qualified Control.Lens as L
import Data.Decimal (Decimal)
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Hledger (
  Status (Cleared),
  Transaction,
  balassert,
  post,
  transaction,
 )
import qualified Hledger.Data.Extra as HDE
import Hledger.Data.Lens (pBalanceAssertion, tDescription, tStatus)
import Hledupt.Data.Currency (chf)
import Hledupt.Data.MyDecimal (
  ChunkSepFormat (NoChunkSep),
  DecimalFormat (..),
  DecimalFractionFormat (TwoDigitDecimalFraction),
  decimalP,
 )
import Hledupt.Wallet (bcgeCCAccount)
import Relude
import Text.Megaparsec (
  MonadParsec (try),
  Parsec,
  anySingle,
  count,
  manyTill,
  manyTill_,
  match,
  parse,
 )
import Text.Megaparsec.Char (char, digitChar, newline, string)
import Text.Megaparsec.Error (errorBundlePretty)

data Rechnung = Rechnung
  { rechnungDay :: !Day
  , rechnungOwedAmount :: !Decimal
  }

type BcgeCCParser = Parsec Void Text

anyLineP :: BcgeCCParser Text
anyLineP = toText <$> manyTill anySingle newline

dateLineP :: BcgeCCParser Day
dateLineP = do
  void $ string "Invoice of "
  (dateString, _) <-
    try $
      match
        (count 2 digitChar >> char '.' >> count 2 digitChar >> char '.' >> many digitChar)
  parseTimeM False defaultTimeLocale "%d.%m.%Y" (toString dateString) <* newline

totalOwedAmountP :: BcgeCCParser Decimal
totalOwedAmountP = do
  void $ string "Total balance in our favour "
  decimalP (DecimalFormat NoChunkSep (Just TwoDigitDecimalFraction)) <* anyLineP

rechnungP :: BcgeCCParser Rechnung
rechnungP = do
  (_, day) <- manyTill_ anyLineP dateLineP
  (_, owedAmount) <- manyTill_ anyLineP totalOwedAmountP
  void $ many anyLineP
  return $
    Rechnung
      { rechnungDay = day
      , rechnungOwedAmount = owedAmount
      }

parseRechnung :: Text -> Either Text Rechnung
parseRechnung rechnung = prepareErrMsg parsedReceipt
 where
  parsedReceipt = parse rechnungP "" rechnung
  prepareErrMsg =
    first (("Could not parse the payslip.\n" <>) . (toText . errorBundlePretty))

rechnungToTransaction :: Rechnung -> Transaction
rechnungToTransaction Rechnung{rechnungDay = day, rechnungOwedAmount = owedAmount} =
  transaction day [balancePosting]
    & L.set tStatus Cleared
      . L.set tDescription "BCGE CC Status"
 where
  balancePosting =
    post bcgeCCAccount (HDE.makeCurrencyAmount chf 0)
      & L.set pBalanceAssertion (balassert . HDE.makeCurrencyAmount chf $ owedAmount)

rechnungToLedger :: Text -> Either Text Transaction
rechnungToLedger rechnungTxt = rechnungToTransaction <$> parseRechnung rechnungTxt
