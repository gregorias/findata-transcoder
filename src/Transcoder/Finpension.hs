module Transcoder.Finpension (
  parsePortfoliosTotal,
) where

import Control.Lens qualified as L
import Data.Decimal (Decimal)
import Data.Decimal.Extra (
  ChunkSepFormat (..),
  DecimalFormat (DecimalFormat),
  DecimalFractionFormat (TwoDigitDecimalFraction),
  decimalP,
 )
import Data.Time (Day)
import Hledger (Status (Cleared), Transaction, balassert)
import Hledger.Data.Extra (
  Comment (NoComment),
  makeCurrencyAmount,
  makePosting,
  makeTransaction,
 )
import Hledger.Data.Lens (pBalanceAssertion)
import Relude
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Extra (parsePretty)
import Transcoder.Data.Currency (chf)
import Transcoder.Wallet (assets, equity, (<:>))

type Parser = MP.Parsec Void Text

portfoliosTotalP :: Parser Decimal
portfoliosTotalP = decimalP (DecimalFormat NoChunkSep (Just TwoDigitDecimalFraction)) <* newline

parsePortfoliosTotal :: Day -> Text -> Either Text Transaction
parsePortfoliosTotal today totalText = do
  total <- parsePretty portfoliosTotalP "Finpension portfolios' total" totalText
  return
    $ makeTransaction
      today
      (Just Cleared)
      "Finpension Status"
      [ makePosting Nothing (assets <:> "Investments:Finpension") Nothing NoComment
          & L.set pBalanceAssertion (balassert . makeCurrencyAmount chf $ total)
      , makePosting Nothing (equity <:> "Finpension Capital Changes") Nothing NoComment
      ]
