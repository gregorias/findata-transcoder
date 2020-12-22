{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Hledupt.Ib
  ( IbData (..),
    showIbData,

    -- * Parsing an activity statement
    ibActivityCsvToLedger,
    parseActivityCsv,
    statementActivityToIbData,
  )
where

import qualified Control.Lens as L
import Data.Decimal (Decimal)
import qualified Data.Map as Map
import Data.Ratio ((%))
import qualified Data.Text as T
import qualified Data.Text as Text
import Data.Time (Day)
import Hledger
  ( Amount,
    AmountPrice (UnitPrice),
    MarketPrice (MarketPrice),
    Posting,
    Status (Cleared, Pending),
    balassert,
    missingamt,
    post,
    setFullPrecision,
  )
import Hledger.Data.Extra (makeCommodityAmount, makeCurrencyAmount)
import Hledger.Data.Lens
  ( aAmountPrice,
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
import Hledupt.Ib.Csv
  ( ActivityStatement (..),
    EndingCash (..),
    StockPosition (StockPosition),
    StockTrade (..),
  )
import qualified Hledupt.Ib.Csv as IbCsv
import Hledupt.Ib.Csv.ActivityStatementParse
  ( BaseCurrency (..),
    QuoteCurrency (..),
    QuotePair (QuotePair),
  )
import Relude
import Text.Printf (printf)

data AssetClass = Stocks | Forex

makeAmount :: AssetClass -> String -> Decimal -> Amount
makeAmount aClass = maker
  where
    maker = case aClass of
      Stocks -> makeCommodityAmount
      Forex -> makeCurrencyAmount

accountPrefix :: AssetClass -> String
accountPrefix Stocks = "Assets:Investments:IB"
accountPrefix Forex = "Assets:Liquid:IB"

accountName :: AssetClass -> String -> Text.Text
accountName assetClass symbol = Text.pack $ accountPrefix assetClass ++ ":" ++ symbol

endingCashToPosting :: EndingCash -> Posting
endingCashToPosting (EndingCash currency amount) =
  post (accountName Forex currency) missingamt
    & L.set pMaybeAmount (Just $ makeAmount Forex currency 0)
      . L.set
        pBalanceAssertion
        (balassert . makeAmount Forex currency $ amount)

stockPositionToPosting :: StockPosition -> Posting
stockPositionToPosting (StockPosition symbol quantity _price) =
  post (accountName Stocks symbol) missingamt
    & L.set pMaybeAmount (Just $ makeAmount Stocks symbol 0)
      . L.set
        pBalanceAssertion
        (balassert . makeAmount Stocks symbol $ fromRational $ quantity % 1)

stockPositionToStockPrice :: Day -> StockPosition -> MarketPrice
stockPositionToStockPrice day (StockPosition sym _q price) =
  MarketPrice day (T.pack sym) (T.pack "USD") price

-- | Ledger representation of Interactive Brokers data found in a Mark-to-Market
-- statement.
data IbData = IbData
  { idStockPrices :: [MarketPrice],
    idTransactions :: [Transaction],
    idStatus :: Maybe Transaction
  }
  deriving stock (Eq, Show)

cashMovementToTransaction :: IbCsv.CashMovement -> Transaction
cashMovementToTransaction
  (IbCsv.CashMovement date currency amount) =
    transaction
      date
      [ post (accountName Forex (show currency)) missingamt
          & L.set pMaybeAmount (Just $ makeAmount Forex (show currency) amount)
            . L.set pStatus Cleared,
        post "Todo" missingamt
          & L.set pStatus Pending
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
    fromList' :: (Ord k) => (a -> k) -> [a] -> Map.Map k a
    fromList' key as = Map.fromList $ map (\a -> (key a, a)) as
    taxMap = fromList' taxToKey taxes

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
      [ post "Assets:Liquid:IB:USD" missingamt,
        post "Assets:Illiquid:IB Withholding Tax" missingamt
          & L.set
            pMaybeAmount
            (Just $ makeAmount Forex "USD" (- taxAmt)),
        post "Income:Capital Gains" missingamt
          & L.set
            pMaybeAmount
            (Just $ makeAmount Forex "USD" (- divAmt))
      ]
      & L.set tDescription title
        . L.set tStatus Cleared
    where
      title = sym ++ " dividend @ " ++ show dps ++ " per share"

stockTradeToTransaction :: StockTrade -> Transaction
stockTradeToTransaction (StockTrade date sym q amount fee) =
  transaction
    date
    [ post (accountName Stocks sym) missingamt
        & L.set
          pMaybeAmount
          (Just $ makeAmount Stocks sym (fromRational $ q % 1)),
      post (accountName Forex "USD") missingamt
        & L.set
          pMaybeAmount
          (Just $ makeAmount Forex "USD" amount),
      post (accountName Forex "USD") (makeAmount Forex "USD" fee),
      post "Expenses:Financial Services" missingamt
        & L.set
          pMaybeAmount
          (Just $ makeAmount Forex "USD" (- fee))
    ]
    & L.set tDescription (sym ++ " trade")
      . L.set tStatus Cleared

forexTradeToTransaction :: IbCsv.ForexTrade -> Transaction
forexTradeToTransaction
  ( IbCsv.ForexTrade
      date
      ( QuotePair
          (BaseCurrency base)
          (QuoteCurrency quote)
        )
      q
      price
      totalCost
      fee
    ) =
    transaction
      date
      [ post
          (accountName Forex base)
          ( makeAmount Forex base (fromRational $ q % 1)
              & L.set
                aAmountPrice
                ( Just . UnitPrice $
                    makeCommodityAmount quote price
                      & setFullPrecision
                )
          ),
        post (accountName Forex quote) (makeAmount Forex quote totalCost),
        post (accountName Forex "CHF") (makeAmount Forex "CHF" fee),
        post "Expenses:Financial Services" (makeAmount Forex "CHF" (- fee))
      ]
      & L.set tDescription (base ++ "." ++ quote)
        . L.set tStatus Cleared

-- | Shows IbData in Ledger format
showIbData :: IbData -> String
showIbData (IbData stockPrices cashMovements maybeStatus) =
  concatMap showTransaction cashMovements
    ++ concatMap showMarketPrice stockPrices
    ++ "\n"
    ++ maybe "" showTransaction maybeStatus

statementActivityToIbData :: IbCsv.ActivityStatement -> Either String IbData
statementActivityToIbData
  ActivityStatement
    { asLastStatementDay = stmtDate,
      asCashPositions = cashes,
      asStockPositions = stocks,
      asCashMovements = cashTransfers,
      asStockTrades = stockTrades,
      asForexTrades = forexTrades,
      asDividends = dividends,
      asTaxes = taxes
    } = do
    dividendsWithTaxes <-
      first unmatchedWithholdingTaxToErrorMessage $
        joinDividendAndTaxRecords dividends taxes
    let cashStatusPostings = map endingCashToPosting cashes
        stockStatusPostings = map stockPositionToPosting stocks
    return $
      IbData
        (stockPositionToStockPrice stmtDate <$> stocks)
        ( sortOn tdate $
            (cashMovementToTransaction <$> cashTransfers)
              ++ (dividendToTransaction <$> dividendsWithTaxes)
              ++ (stockTradeToTransaction <$> stockTrades)
              ++ (forexTradeToTransaction <$> forexTrades)
        )
        ( do
            guard $ not (null cashStatusPostings && null stockStatusPostings)
            return $
              transaction
                stmtDate
                (cashStatusPostings ++ stockStatusPostings)
                & L.set tDescription "IB Status"
                  . L.set tStatus Cleared
        )

-- | Parses an IB Activity CSV statement into a Ledger status transaction
parseActivityCsv :: String -> Either String IbData
parseActivityCsv csv = IbCsv.parseActivity csv >>= statementActivityToIbData

-- | Parses an IB Activity CSV statement into a Ledger text file.
ibActivityCsvToLedger :: String -> Either String String
ibActivityCsvToLedger = fmap showIbData . parseActivityCsv
