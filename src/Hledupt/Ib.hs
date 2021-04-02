{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Hledupt.Ib (
  -- * Parsing an activity statement
  parseActivityCsv,
  statementActivityToLedgerReport,
) where

import qualified Control.Lens as L
import Data.Decimal (Decimal)
import qualified Data.Map as Map
import Data.Ratio ((%))
import qualified Data.Text as T
import qualified Data.Text as Text
import Data.Time (Day)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import Hledger (
  AmountPrice (UnitPrice),
  MarketPrice (MarketPrice),
  Posting,
  Status (Cleared, Pending),
  amountSetFullPrecision,
  balassert,
  missingamt,
  post,
 )
import Hledger.Data.Extra (makeCommodityAmount, makeCurrencyAmount)
import Hledger.Data.Lens (
  aAmountPrice,
  pBalanceAssertion,
  pMaybeAmount,
  pStatus,
  tDescription,
  tStatus,
 )
import Hledger.Data.Transaction (transaction)
import Hledger.Data.Types (
  Transaction (..),
 )
import Hledupt.Data.Currency (Currency (CHF, USD))
import Hledupt.Data.LedgerReport (LedgerReport (..))
import Hledupt.Ib.Csv (
  ActivityStatement (..),
  EndingCash (..),
  StockPosition (StockPosition),
  StockTrade (..),
 )
import qualified Hledupt.Ib.Csv as IbCsv
import Hledupt.Ib.Csv.ActivityStatementParse (
  BaseCurrency (..),
  QuoteCurrency (..),
  QuotePair (QuotePair),
 )
import Relude
import Text.Printf (printf)

data AssetClass = Stocks | Forex

accountPrefix :: AssetClass -> String
accountPrefix Stocks = "Assets:Investments:IB"
accountPrefix Forex = "Assets:Liquid:IB"

cashAccountName :: Currency -> Text.Text
cashAccountName cur = Text.pack $ accountPrefix Forex ++ ":" ++ show cur

stockAccountName :: String -> Text.Text
stockAccountName symbol = Text.pack $ accountPrefix Stocks ++ ":" ++ symbol

endingCashToPosting :: EndingCash -> Posting
endingCashToPosting (EndingCash currency amount) =
  post (cashAccountName currency) missingamt
    & L.set pMaybeAmount (Just $ makeCurrencyAmount currency 0)
      . L.set
        pBalanceAssertion
        (balassert . makeCurrencyAmount currency $ amount)

stockPositionToPosting :: StockPosition -> Posting
stockPositionToPosting (StockPosition symbol quantity _price) =
  post (stockAccountName symbol) missingamt
    & L.set pMaybeAmount (Just $ makeCommodityAmount symbol 0)
      . L.set
        pBalanceAssertion
        (balassert . makeCommodityAmount symbol $ fromRational $ quantity % 1)

stockPositionToStockPrice :: Day -> StockPosition -> MarketPrice
stockPositionToStockPrice day (StockPosition sym _q price) =
  MarketPrice day (T.pack sym) (T.pack "USD") price

cashMovementToTransaction :: IbCsv.CashMovement -> Transaction
cashMovementToTransaction
  (IbCsv.CashMovement date currency amount) =
    transaction
      date
      [ post (cashAccountName currency) missingamt
          & L.set pMaybeAmount (Just $ makeCurrencyAmount currency amount)
            . L.set pStatus Cleared
      , post "Todo" missingamt
          & L.set pStatus Pending
      ]
      & L.set tDescription "IB Deposit/Withdrawal"

data DividendWithTax = DividendWithTax
  { dDate :: Day
  , dSymbol :: String
  , dDividendPerShare :: Decimal
  , dTotalDividendAmount :: Decimal
  , dTotalTaxAmount :: Decimal
  }

dividentToDividendWithTax :: IbCsv.Dividend -> DividendWithTax
dividentToDividendWithTax dividend =
  DividendWithTax
    { dDate = IbCsv.dDate dividend
    , dSymbol = IbCsv.dSymbol dividend
    , dDividendPerShare = IbCsv.dDividendPerShare dividend
    , dTotalDividendAmount = IbCsv.dTotalAmount dividend
    , dTotalTaxAmount = fromRational 0
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
  { _uwtDate :: Day
  , _uwtSymbol :: String
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
    { dDate = d
    , dSymbol = sym
    , dDividendPerShare = dps
    , dTotalDividendAmount = divAmt
    , dTotalTaxAmount = taxAmt
    } =
    transaction
      d
      [ post "Assets:Liquid:IB:USD" missingamt
      , post whTaxTitle missingamt
          & L.set
            pMaybeAmount
            (Just $ makeCurrencyAmount USD (- taxAmt))
      , post "Income:Capital Gains" missingamt
          & L.set
            pMaybeAmount
            (Just $ makeCurrencyAmount USD (- divAmt))
      ]
      & L.set tDescription title
        . L.set tStatus Cleared
   where
    title = sym ++ " dividend @ " ++ show dps ++ " per share"
    whTaxTitle =
      "State:"
        `Text.append` (Text.pack . show . fst . toOrdinalDate $ d)
        `Text.append` ":IB Withholding Tax:"
        `Text.append` Text.pack sym

stockTradeToTransaction :: StockTrade -> Transaction
stockTradeToTransaction (StockTrade date sym q amount fee) =
  transaction
    date
    [ post (stockAccountName sym) missingamt
        & L.set
          pMaybeAmount
          (Just $ makeCommodityAmount sym (fromRational $ q % 1))
    , post (cashAccountName USD) missingamt
        & L.set
          pMaybeAmount
          (Just $ makeCurrencyAmount USD amount)
    , post (cashAccountName USD) (makeCurrencyAmount USD fee)
    , post "Expenses:Financial Services" missingamt
        & L.set
          pMaybeAmount
          (Just $ makeCurrencyAmount USD (- fee))
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
          (cashAccountName base)
          ( makeCurrencyAmount base q
              & L.set
                aAmountPrice
                ( Just . UnitPrice $
                    makeCommodityAmount (show quote) price
                      & amountSetFullPrecision
                )
          )
      , post (cashAccountName quote) (makeCurrencyAmount quote totalCost)
      , post (cashAccountName CHF) (makeCurrencyAmount CHF fee)
      , post "Expenses:Financial Services" (makeCurrencyAmount CHF (- fee))
      ]
      & L.set tDescription (show base ++ "." ++ show quote)
        . L.set tStatus Cleared

statementActivityToLedgerReport ::
  IbCsv.ActivityStatement ->
  Either String LedgerReport
statementActivityToLedgerReport
  ActivityStatement
    { asLastStatementDay = stmtDate
    , asCashPositions = cashes
    , asStockPositions = stocks
    , asCashMovements = cashTransfers
    , asStockTrades = stockTrades
    , asForexTrades = forexTrades
    , asDividends = dividends
    , asTaxes = taxes
    } = do
    dividendsWithTaxes <-
      first unmatchedWithholdingTaxToErrorMessage $
        joinDividendAndTaxRecords dividends taxes
    let cashStatusPostings = map endingCashToPosting cashes
        stockStatusPostings = map stockPositionToPosting stocks
        maybeStatus =
          ( do
              guard $ not (null cashStatusPostings && null stockStatusPostings)
              return $
                transaction
                  stmtDate
                  (cashStatusPostings ++ stockStatusPostings)
                  & L.set tDescription "IB Status"
                    . L.set tStatus Cleared
          )
    return $
      LedgerReport
        ( sortOn tdate $
            (cashMovementToTransaction <$> cashTransfers)
              ++ (dividendToTransaction <$> dividendsWithTaxes)
              ++ (stockTradeToTransaction <$> stockTrades)
              ++ (forexTradeToTransaction <$> forexTrades)
              ++ maybeStatus
        )
        (stockPositionToStockPrice stmtDate <$> stocks)

-- | Parses an IB Activity CSV statement into a Ledger status transaction
parseActivityCsv :: String -> Either String LedgerReport
parseActivityCsv csv = IbCsv.parseActivity csv >>= statementActivityToLedgerReport
