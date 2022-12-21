{-# OPTIONS_GHC -Wno-typed-holes #-}

module Transcoder.Finpension (
  parsePortfoliosTotal,
) where

import qualified Control.Lens as L
import Data.Decimal (Decimal)
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
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Extra (parsePretty)
import Transcoder.Data.Currency (chf)
import Transcoder.Data.MyDecimal (
  ChunkSepFormat (..),
  DecimalFormat (DecimalFormat),
  DecimalFractionFormat (TwoDigitDecimalFraction),
  decimalP,
 )
import Transcoder.Wallet (assets, equity, (<:>))

type Parser = MP.Parsec Void Text

portfoliosTotalP :: Parser Decimal
portfoliosTotalP = decimalP (DecimalFormat NoChunkSep (Just TwoDigitDecimalFraction)) <* newline

parsePortfoliosTotal :: Day -> Text -> Either Text Transaction
parsePortfoliosTotal today totalText = do
  total <- parsePretty portfoliosTotalP "Finpension portfolios' total" totalText
  return $
    makeTransaction
      today
      (Just Cleared)
      "Finpension Status"
      [ makePosting Nothing (assets <:> "Investments:Finpension") Nothing NoComment
          & L.set pBalanceAssertion (balassert . makeCurrencyAmount chf $ total)
      , makePosting Nothing (equity <:> "Finpension Capital Changes") Nothing NoComment
      ]
