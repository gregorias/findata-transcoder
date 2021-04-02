{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Hledupt.Finpension (
  transactionsToLedger,
) where

import qualified Data.ByteString.Lazy as LBS
import Data.Currency (Alpha)
import Data.Fixed (E6, Fixed)
import Data.Time (Day)
import Hledupt.Data.CsvFile (CsvFile)
import Hledupt.Data.LedgerReport (LedgerReport)
import Relude

finpensionFunds :: [(Text, Text)]
finpensionFunds =
  [ ("CSIF (CH) III Equity World ex CH Blue - Pension Fund ZB", "CH0130458182")
  , ("CSIF (CH) III Equity World ex CH Small Cap Blue - Pension Fund DB", "CH0017844686")
  , ("CSIF (CH) Equity Emerging Markets Blue DB", "CH0214967314")
  , ("CSIF (CH) Equity Switzerland Small & Mid Cap ZB", "CH0033782431")
  , ("CSIF (CH) Equity Switzerland Large Cap Blue ZB", "CH0110869143")
  ]

data Category = Buy | Deposit

data Transaction = Transaction
  { trDate :: !Day
  , trCategory :: !Category
  , trAssetName :: !Text
  , trNumberOfShares :: !(Fixed E6)
  , trAssetCurrency :: !Alpha
  , trCurrencyRate :: !(Fixed E6)
  , trAssetPriceInChf :: !(Fixed E6)
  , trCashFlow :: !(Fixed E6)
  , trBalance :: !(Fixed E6)
  }

transactionsToLedger :: CsvFile LBS.ByteString -> Either Text LedgerReport
transactionsToLedger = const . Left $ "unimplemented"
