{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Hledupt.Finpension (
  funds,
  transactionsToLedger,
) where

import Control.Lens (each)
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy as LBS
import Data.Currency (Alpha)
import Data.Fixed (E6, Fixed)
import Data.Time (Day)
import qualified Hledger as Ledger
import Hledupt.Data.CsvFile (CsvFile)
import Hledupt.Data.Isin (Isin, mkIsin)
import Hledupt.Data.LedgerReport (LedgerReport)
import Relude
import Relude.Unsafe (fromJust)

data Fund = Fund
  { fundFinpensionName :: !Text
  , fundIsin :: !Isin
  , fundShortName :: !Text
  }
  deriving stock (Show)

funds :: [Fund]
funds =
  fromJust $
    fmap (\(a, b, c) -> Fund a b c)
      <$> L.traverseOf (each . L._2) mkIsin sourcelist
 where
  sourcelist =
    [
      ( "CSIF (CH) III Equity World ex CH Blue - Pension Fund ZB"
      , "CH0130458182"
      , "World ex CH"
      )
    ,
      ( "CSIF (CH) III Equity World ex CH Small Cap Blue - Pension Fund DB"
      , "CH0017844686"
      , "World ex CH Small Cap"
      )
    ,
      ( "CSIF (CH) Equity Emerging Markets Blue DB"
      , "CH0214967314"
      , "Emerging Markets"
      )
    ,
      ( "CSIF (CH) Equity Switzerland Small & Mid Cap ZB"
      , "CH0033782431"
      , "CH Small & Mid Cap"
      )
    ,
      ( "CSIF (CH) Equity Switzerland Large Cap Blue ZB"
      , "CH0110869143"
      , "Large Cap"
      )
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

transactionsP :: CsvFile LBS.ByteString -> Either Text [Transaction]
transactionsP = const . Left $ "unimplemented"

finpensionTransactionToLedgerTransaction :: Transaction -> Ledger.Transaction
finpensionTransactionToLedgerTransaction = undefined

transactionsToLedger :: CsvFile LBS.ByteString -> Either Text LedgerReport
transactionsToLedger csv = do
  _trs <- transactionsP csv
  Left "unimplemented"
