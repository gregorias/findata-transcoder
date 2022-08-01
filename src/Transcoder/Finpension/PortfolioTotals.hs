{-# OPTIONS_GHC -Wno-typed-holes #-}

module Transcoder.Finpension.PortfolioTotals (
  portfolioTotalsToStatusTransaction,
) where

import qualified Control.Lens as L
import Data.Cash (Cash (Cash))
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
import Text.Megaparsec.Char (digitChar, newline, space, string)
import Text.Megaparsec.Extra (parsePretty)
import Transcoder.Data.Currency (chf)
import Transcoder.Data.MyDecimal (ChunkSepFormat (ChunkSep), DecimalFormat (DecimalFormat), DecimalFractionFormat (TwoDigitDecimalFraction), decimalP)
import Transcoder.Wallet (assets, (<:>))

type Parser = MP.Parsec Void Text

portfolioTotalLineP :: Parser Decimal
portfolioTotalLineP = do
  void $ string "Portfolio" >> space >> some digitChar >> space
  total <- decimalP (DecimalFormat (ChunkSep '\'') (Just TwoDigitDecimalFraction))
  void $ space >> string "CHF" >> newline
  return total

portfolioTotalsP :: Parser Decimal
portfolioTotalsP = do
  totals <- MP.many portfolioTotalLineP
  return $ sum totals

portfolioTotalsToStatusTransaction :: Day -> Text -> Either Text Transaction
portfolioTotalsToStatusTransaction today text = do
  total <- parsePretty portfolioTotalsP "Finpension portfolio totals" text
  return $
    makeTransaction
      today
      (Just Cleared)
      "Finpension Status"
      [ makePosting Nothing (assets <:> "Investments:Finpension") (Just $ Cash chf 0) NoComment
          & L.set pBalanceAssertion (balassert . makeCurrencyAmount chf $ total)
      ]
