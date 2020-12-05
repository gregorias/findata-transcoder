{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Hledupt.Ib
  ( ibCsvToLedger,
    parseCsv,
    IbData (..),
    showIbData,
  )
where

import qualified Control.Lens as L
import Control.Monad (guard)
import Data.Decimal (Decimal)
import Data.Function ((&))
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Time (Day)
import Hledger (Amount, MarketPrice (MarketPrice), Posting, balassert, nullposting)
import Hledger.Data.Extra (makeCommodityAmount, makeCurrencyAmount)
import Hledger.Data.Lens
  ( pAccount,
    pBalanceAssertion,
    pMaybeAmount,
    tDescription,
  )
import Hledger.Data.MarketPrice.Extra (showMarketPrice)
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

positionRecordToPosting :: IbCsv.PositionRecord -> Posting
positionRecordToPosting record = do
  let assetClass = csvAssetClassToLedgerAssetClass . IbCsv.assetClass $ record
      symbol = IbCsv.symbol record
      accountName = accountPrefix assetClass ++ ":" ++ IbCsv.symbol record
   in nullposting
        & L.set pAccount accountName
          . L.set pMaybeAmount (Just $ makeAmount assetClass symbol 0)
          . L.set
            pBalanceAssertion
            (balassert . makeAmount assetClass symbol $ IbCsv.quantity record)

positionRecordToStockPrice :: Day -> IbCsv.PositionRecord -> Maybe MarketPrice
positionRecordToStockPrice day rec = do
  guard $ IbCsv.assetClass rec == IbCsv.Stocks
  return $
    MarketPrice
      day
      (T.pack $ IbCsv.symbol rec)
      (T.pack . show . IbCsv.currency $ rec)
      (IbCsv.price rec)

data IbData = IbData
  { stockPrices :: [MarketPrice],
    status :: Transaction
  }
  deriving (Eq, Show)

-- | Shows IbData in Ledger format
showIbData :: IbData -> String
showIbData (IbData stockPrices status) =
  concatMap showMarketPrice stockPrices ++ "\n" ++ showTransaction status

-- | Parses IB MtoM CSV statement into a Ledger status transaction
parseCsv :: String -> Either String IbData
parseCsv csv = do
  statement <- IbCsv.parse csv
  let posRecords = IbCsv.positionRecords statement
      statementDay = IbCsv.lastStatementDay statement
  return $
    IbData
      (mapMaybe (positionRecordToStockPrice statementDay) posRecords)
      ( transaction
          (show statementDay)
          (fmap positionRecordToPosting posRecords)
          & L.set tDescription "IB Status"
      )

ibCsvToLedger :: String -> Either String String
ibCsvToLedger = fmap showIbData . parseCsv
