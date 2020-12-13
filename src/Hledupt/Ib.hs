{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Hledupt.Ib
  ( ibCsvToLedger,
    parseCsv,
    IbData (..),
    showIbData,
    statementToIbData,
  )
where

import qualified Control.Lens as L
import Control.Monad (guard)
import Data.Bifunctor (Bifunctor (first))
import Data.Decimal (Decimal)
import Data.Function ((&))
import Data.List (sortOn)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, mapMaybe)
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
import Hledupt.Data (MonetaryValue)
import qualified Hledupt.Ib.Csv as IbCsv
import Text.Printf (printf)

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

-- | Ledger representation of Interactive Brokers data found in a Mark-to-Market
-- statement.
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
      date
      [ nullposting
          & L.set pAccount (accountName Forex (show currency))
            . L.set pMaybeAmount (Just $ makeAmount Forex (show currency) amount)
            . L.set pStatus Cleared,
        nullposting
          & L.set pAccount "Todo"
            . L.set pStatus Pending
      ]
      & L.set tDescription "IB Deposit/Withdrawal"

data DividendWithTax = DividendWithTax
  { dDate :: Day,
    dSymbol :: String,
    dDividendPerShare :: MonetaryValue,
    dTotalDividendAmount :: MonetaryValue,
    dTotalTaxAmount :: MonetaryValue
  }

dividentToDividendWithTax :: IbCsv.Dividend -> DividendWithTax
dividentToDividendWithTax dividend =
  DividendWithTax
    { dDate = IbCsv.dDate dividend,
      dSymbol = IbCsv.dSymbol dividend,
      dDividendPerShare = IbCsv.dDividendPerShare dividend,
      dTotalDividendAmount = IbCsv.dTotalAmount dividend,
      dTotalTaxAmount = fromRational 0
    }

mergeDividendWithTax ::
  IbCsv.Dividend ->
  IbCsv.WithholdingTax ->
  DividendWithTax
mergeDividendWithTax dividend tax =
  (dividentToDividendWithTax dividend)
    { dTotalTaxAmount = IbCsv.wtTotalAmount tax
    }

data UnmatchedWithholdingTax = UnmatchedWithholdingTax
  { _uwtDate :: Day,
    _uwtSymbol :: String
  }

withholdingTaxToUnmatchedWithholdingTax :: IbCsv.WithholdingTax -> UnmatchedWithholdingTax
withholdingTaxToUnmatchedWithholdingTax (IbCsv.WithholdingTax day symbol _) =
  UnmatchedWithholdingTax day symbol

unmatchedWithholdingTaxToErrorMessage ::
  UnmatchedWithholdingTax -> String
unmatchedWithholdingTaxToErrorMessage (UnmatchedWithholdingTax date symbol) =
  printf
    "Could not find a dividend match for %s withholding tax from %s.\
    \ This could happen due to IB statement cut-off (fetch a broader \
    \statement) or wrong data assumptions."
    symbol
    (show date)

type Key = (Day, String)

type Accum = (Map.Map Key IbCsv.WithholdingTax, [DividendWithTax])

joinDividendAndTaxRecords ::
  [IbCsv.Dividend] ->
  [IbCsv.WithholdingTax] ->
  Either UnmatchedWithholdingTax [DividendWithTax]
joinDividendAndTaxRecords divs taxes =
  maybe
    (pure dwts)
    (Left . withholdingTaxToUnmatchedWithholdingTax)
    remainingTax
  where
    dividendToKey dividend = (IbCsv.dDate dividend, IbCsv.dSymbol dividend)
    taxToKey tax = (IbCsv.wtDate tax, IbCsv.wtSymbol tax)
    fromList :: (Ord k) => (a -> k) -> [a] -> Map.Map k a
    fromList key as = Map.fromList $ map (\a -> (key a, a)) as
    taxMap = fromList taxToKey taxes

    update :: (IbCsv.Dividend -> Accum -> Accum)
    update dividend (m, dwts') = (m', newDwt : dwts')
      where
        divKey = dividendToKey dividend
        (maybeTax, m') = Map.updateLookupWithKey (\_ _ -> Nothing) divKey m
        newDwt = case maybeTax of
          Just tax -> mergeDividendWithTax dividend tax
          Nothing -> dividentToDividendWithTax dividend
    (remainingTaxMap, dwts) = foldr update (taxMap, []) divs
    remainingTax = listToMaybe $ Map.elems remainingTaxMap

dividendToTransaction :: DividendWithTax -> Transaction
dividendToTransaction
  DividendWithTax
    { dDate = d,
      dSymbol = sym,
      dDividendPerShare = dps,
      dTotalDividendAmount = divAmt,
      dTotalTaxAmount = taxAmt
    } =
    transaction
      d
      [ nullposting
          & L.set pAccount "Assets:Liquid:IB:USD",
        nullposting
          & L.set pAccount "Assets:Illiquid:IB Withholding Tax"
            . L.set
              pMaybeAmount
              (Just $ makeAmount Forex "USD" (- taxAmt)),
        nullposting
          & L.set pAccount "Income:Capital Gains"
            . L.set
              pMaybeAmount
              (Just $ makeAmount Forex "USD" (- divAmt))
      ]
      & L.set tDescription title
        . L.set tStatus Cleared
    where
      title = sym ++ " dividend @ " ++ show dps ++ " per share"

-- | Shows IbData in Ledger format
showIbData :: IbData -> String
showIbData (IbData stockPrices cashMovements maybeStatus) =
  concatMap showTransaction cashMovements
    ++ concatMap showMarketPrice stockPrices
    ++ "\n"
    ++ maybe "" showTransaction maybeStatus

statementToIbData :: IbCsv.Statement -> Either String IbData
statementToIbData
  ( IbCsv.Statement
      statementDay
      posRecords
      cashMovements
      dividendRecords
      withholdingTaxRecords
    ) = do
    let statusPostings = fmap positionRecordToStatusPosting posRecords
        cmTrs = map cashMovementToTransaction cashMovements
        dividends =
          mapMaybe
            ( \case
                IbCsv.DividendRecord dividend -> Just dividend
                IbCsv.TotalDividendsRecord -> Nothing
            )
            dividendRecords
        taxes =
          mapMaybe
            ( \case
                IbCsv.WithholdingTaxRecord tax -> Just tax
                IbCsv.TotalWithholdingTaxRecord -> Nothing
            )
            withholdingTaxRecords
    dividendsWithTaxes <-
      first unmatchedWithholdingTaxToErrorMessage $
        joinDividendAndTaxRecords dividends taxes
    return $
      IbData
        (mapMaybe (positionRecordToStockPrice statementDay) posRecords)
        ( sortOn tdate $
            cmTrs
              ++ fmap dividendToTransaction dividendsWithTaxes
        )
        ( do
            guard $ not (null statusPostings)
            return $
              transaction
                statementDay
                statusPostings
                & L.set tDescription "IB Status"
                  . L.set tStatus Cleared
        )

-- | Parses IB MtoM CSV statement into a Ledger status transaction
parseCsv :: String -> Either String IbData
parseCsv csv = IbCsv.parse csv >>= statementToIbData

-- | Parses IB MtoM CSV statement into a Ledger text file.
ibCsvToLedger :: String -> Either String String
ibCsvToLedger = fmap showIbData . parseCsv
