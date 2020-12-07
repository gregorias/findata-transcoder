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
import Hledger
  ( Amount,
    MarketPrice (MarketPrice),
    Posting,
    Status (Cleared, Pending),
    balassert,
    nullposting,
  )
import Hledger.Data.Extra (makeCommodityAmount, makeCurrencyAmount)
import Hledger.Data.Lens
  ( pAccount,
    pBalanceAssertion,
    pMaybeAmount,
    pStatus,
    tDescription,
    tStatus,
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

accountName :: AssetClass -> String -> String
accountName assetClass symbol = accountPrefix assetClass ++ ":" ++ symbol

positionRecordToStatusPosting :: IbCsv.PositionRecord -> Posting
positionRecordToStatusPosting record =
  let assetClass = csvAssetClassToLedgerAssetClass . IbCsv.prAssetClass $ record
      symbol = IbCsv.prSymbol record
   in nullposting
        & L.set pAccount (accountName assetClass symbol)
          . L.set pMaybeAmount (Just $ makeAmount assetClass symbol 0)
          . L.set
            pBalanceAssertion
            (balassert . makeAmount assetClass symbol $ IbCsv.prQuantity record)

positionRecordToStockPrice :: Day -> IbCsv.PositionRecord -> Maybe MarketPrice
positionRecordToStockPrice day rec = do
  guard $ IbCsv.prAssetClass rec == IbCsv.Stocks
  -- Records of stocks that I do not own miss the price data.
  guard $ IbCsv.prQuantity rec > 0
  return $
    MarketPrice
      day
      (T.pack $ IbCsv.prSymbol rec)
      (T.pack . show . IbCsv.prCurrency $ rec)
      (IbCsv.prPrice rec)

data IbData = IbData
  { idStockPrices :: [MarketPrice],
    idTransactions :: [Transaction],
    idStatus :: Maybe Transaction
  }
  deriving (Eq, Show)

cashMovementToTransaction :: IbCsv.CashMovement -> Transaction
cashMovementToTransaction
  (IbCsv.CashMovement date currency amount) =
    transaction
      (show date)
      [ nullposting
          & L.set pAccount (accountName Forex (show currency))
            . L.set pMaybeAmount (Just $ makeAmount Forex (show currency) amount)
            . L.set pStatus Cleared,
        nullposting
          & L.set pAccount "Todo"
            . L.set pStatus Pending
      ]
      & L.set tDescription "IB Deposit/Withdrawal"

-- | Shows IbData in Ledger format
showIbData :: IbData -> String
showIbData (IbData stockPrices cashMovements maybeStatus) =
  concatMap showTransaction cashMovements
    ++ concatMap showMarketPrice stockPrices
    ++ "\n"
    ++ maybe "" showTransaction maybeStatus

-- | Parses IB MtoM CSV statement into a Ledger status transaction
parseCsv :: String -> Either String IbData
parseCsv csv = do
  IbCsv.Statement statementDay posRecords cashMovements <- IbCsv.parse csv
  let statusPostings = fmap positionRecordToStatusPosting posRecords
  return $
    IbData
      (mapMaybe (positionRecordToStockPrice statementDay) posRecords)
      (map cashMovementToTransaction cashMovements)
      ( do
          guard $ not (null statusPostings)
          return $
            transaction
              (show statementDay)
              statusPostings
              & L.set tDescription "IB Status"
                . L.set tStatus Cleared
      )

ibCsvToLedger :: String -> Either String String
ibCsvToLedger = fmap showIbData . parseCsv
