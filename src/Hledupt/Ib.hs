{-# LANGUAGE ExtendedDefaultRules #-}

module Hledupt.Ib (
  -- * Parsing an activity statement
  parseActivityCsv,
  statementActivityToLedgerReport,
) where

import qualified Control.Lens as L
import Data.Decimal (Decimal)
import qualified Data.Map as Map
import Data.Ratio ((%))
import Data.Time (Day)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import Hledger (
  AmountPrice (UnitPrice),
  MarketPrice (MarketPrice),
  Posting,
  Status (Cleared),
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
import Hledupt.Data.Currency (Currency, chf, usd)
import Hledupt.Data.LedgerReport (LedgerReport (..), todoPosting)
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

accountPrefix :: AssetClass -> Text
accountPrefix Stocks = "Assets:Investments:IB"
accountPrefix Forex = "Assets:Liquid:IB"

cashAccountName :: Currency -> Text
cashAccountName cur = accountPrefix Forex <> ":" <> show cur

stockAccountName :: Text -> Text
stockAccountName symbol = accountPrefix Stocks <> ":" <> symbol

endingCashToPosting :: EndingCash -> Posting
endingCashToPosting (EndingCash currency amount) =
  post (cashAccountName currency) missingamt
    & L.set pMaybeAmount (Just $ makeCurrencyAmount currency 0)
      . L.set
        pBalanceAssertion
        (balassert . makeCurrencyAmount currency $ amount)

stockPositionToPosting :: StockPosition -> Posting
stockPositionToPosting (StockPosition symbol quantity _price) =
  post (stockAccountName . toText $ symbol) missingamt
    & L.set pMaybeAmount (Just $ makeCommodityAmount (toText symbol) 0)
      . L.set
        pBalanceAssertion
        (balassert . makeCommodityAmount (toText symbol) $ fromRational $ quantity % 1)

stockPositionToStockPrice :: Day -> StockPosition -> MarketPrice
stockPositionToStockPrice day (StockPosition sym _q price) =
  MarketPrice day (toText sym) "USD" price

cashMovementToTransaction :: IbCsv.CashMovement -> Transaction
cashMovementToTransaction
  (IbCsv.CashMovement date currency amount) =
    transaction
      date
      [ post (cashAccountName currency) missingamt
          & L.set pMaybeAmount (Just $ makeCurrencyAmount currency amount)
            . L.set pStatus Cleared
      , todoPosting
      ]
      & L.set tDescription "IB Deposit/Withdrawal"

data DividendWithTax = DividendWithTax
  { dDate :: Day
  , dSymbol :: !Text
  , dDividendPerShare :: Decimal
  , dTotalDividendAmount :: Decimal
  , dTotalTaxAmount :: Decimal
  }

dividentToDividendWithTax :: IbCsv.Dividend -> DividendWithTax
dividentToDividendWithTax dividend =
  DividendWithTax
    { dDate = IbCsv.dDate dividend
    , dSymbol = toText $ IbCsv.dSymbol dividend
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
  , _uwtSymbol :: !Text
  }

withholdingTaxToUnmatchedWithholdingTax :: IbCsv.WithholdingTax -> UnmatchedWithholdingTax
withholdingTaxToUnmatchedWithholdingTax (IbCsv.WithholdingTax day symbol _) =
  UnmatchedWithholdingTax day (toText symbol)

unmatchedWithholdingTaxToErrorMessage ::
  UnmatchedWithholdingTax -> Text
unmatchedWithholdingTaxToErrorMessage (UnmatchedWithholdingTax date symbol) =
  toText @String $
    printf
      "Could not find a dividend match for %s withholding tax from %s.\
      \ This could happen due to IB statement cut-off (fetch a broader \
      \statement) or wrong data assumptions."
      symbol
      (show date)

type Key = (Day, Text)

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
  dividendToKey dividend = (IbCsv.dDate dividend, toText $ IbCsv.dSymbol dividend)
  taxToKey tax = (IbCsv.wtDate tax, toText $ IbCsv.wtSymbol tax)
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
            (Just $ makeCurrencyAmount usd (- taxAmt))
      , post "Income:Capital Gains" missingamt
          & L.set
            pMaybeAmount
            (Just $ makeCurrencyAmount usd (- divAmt))
      ]
      & L.set tDescription title
        . L.set tStatus Cleared
   where
    title = sym <> " dividend @ " <> show dps <> " per share"
    whTaxTitle =
      "State:"
        <> (show . fst . toOrdinalDate $ d)
        <> ":IB Withholding Tax:"
        <> sym

stockTradeToTransaction :: StockTrade -> Transaction
stockTradeToTransaction (StockTrade date sym q amount fee) =
  transaction
    date
    [ post (stockAccountName . toText $ sym) missingamt
        & L.set
          pMaybeAmount
          (Just $ makeCommodityAmount (toText sym) (fromRational $ q % 1))
    , post (cashAccountName usd) missingamt
        & L.set
          pMaybeAmount
          (Just $ makeCurrencyAmount usd amount)
    , post (cashAccountName usd) (makeCurrencyAmount usd fee)
    , post "Expenses:Financial Services" missingamt
        & L.set
          pMaybeAmount
          (Just $ makeCurrencyAmount usd (- fee))
    ]
    & L.set tDescription (toText sym <> " trade")
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
      , post (cashAccountName chf) (makeCurrencyAmount chf fee)
      , post "Expenses:Financial Services" (makeCurrencyAmount chf (- fee))
      ]
      & L.set tDescription (show base <> "." <> show quote)
        . L.set tStatus Cleared

statementActivityToLedgerReport ::
  IbCsv.ActivityStatement ->
  Either Text LedgerReport
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
                  (cashStatusPostings <> stockStatusPostings)
                  & L.set tDescription "IB Status"
                    . L.set tStatus Cleared
          )
    return $
      LedgerReport
        ( sortOn tdate $
            (cashMovementToTransaction <$> cashTransfers)
              <> (dividendToTransaction <$> dividendsWithTaxes)
              <> (stockTradeToTransaction <$> stockTrades)
              <> (forexTradeToTransaction <$> forexTrades)
              <> maybeStatus
        )
        (stockPositionToStockPrice stmtDate <$> stocks)

-- | Parses an IB Activity CSV statement into a Ledger status transaction
parseActivityCsv :: Text -> Either Text LedgerReport
parseActivityCsv csv = IbCsv.parseActivity csv >>= statementActivityToLedgerReport
