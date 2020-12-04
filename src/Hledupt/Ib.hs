{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Hledupt.Ib
  ( ibCsvToLedger,
    parseCsv,
  )
where

import qualified Control.Lens as L
import Data.Decimal (Decimal)
import Data.Function ((&))
import Data.Maybe (mapMaybe)
import Hledger (Amount, Posting, balassert, nullposting)
import Hledger.Data.Extra (makeCommodityAmount, makeCurrencyAmount)
import Hledger.Data.Lens
  ( pAccount,
    pBalanceAssertion,
    pMaybeAmount,
    tDescription,
  )
import Hledger.Data.Transaction (showTransaction, transaction)
import Hledger.Data.Types
  ( Transaction (..),
  )
import qualified Hledupt.Ib.Csv as IbCsv

data AssetClass = Stocks | Forex

csvAssetClassToLedgerAssetClass :: IbCsv.PositionRecordAssetClass -> AssetClass
csvAssetClassToLedgerAssetClass IbCsv.Stocks = Stocks
csvAssetClassToLedgerAssetClass IbCsv.Forex = Forex

makeAmount :: AssetClass -> String -> Decimal -> Amount
makeAmount aClass = maker
  where
    maker = case aClass of
      Stocks -> makeCommodityAmount
      Forex -> makeCurrencyAmount

accountPrefix :: AssetClass -> String
accountPrefix Stocks = "Assets:Investments:IB"
accountPrefix Forex = "Assets:Liquid:IB"

positionRecordToPosting :: IbCsv.PositionRecord -> Maybe Posting
positionRecordToPosting record = do
  let assetClass = csvAssetClassToLedgerAssetClass . IbCsv.assetClass $ record
  let symbol = IbCsv.symbol record
  let accountName = accountPrefix assetClass ++ ":" ++ IbCsv.symbol record
  return $
    nullposting
      & L.set pAccount accountName
        . L.set pMaybeAmount (Just $ makeAmount assetClass symbol 0)
        . L.set
          pBalanceAssertion
          (balassert . makeAmount assetClass symbol $ IbCsv.quantity record)

-- | Parses IB MtoM CSV statement into a Ledger status transaction
parseCsv :: String -> Either String Transaction
parseCsv csv = do
  statement <- IbCsv.parse csv
  let posRecords = IbCsv.positionRecords statement
  return $
    transaction
      (show . IbCsv.lastStatementDay $ statement)
      (mapMaybe positionRecordToPosting posRecords)
      & L.set tDescription "IB Status"

ibCsvToLedger :: String -> Either String String
ibCsvToLedger = fmap showTransaction . parseCsv
