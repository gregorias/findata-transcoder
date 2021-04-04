{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | This module parses Finpension's reports and produces Ledger reports.
module Hledupt.Finpension (
  funds,
  transactionsToLedger,
) where

import Control.Lens (each, over, set)
import qualified Control.Lens as L
import Control.Lens.Internal.ByteString (unpackStrict8)
import qualified Data.Csv as CSV
import Data.Currency (Alpha)
import Data.Decimal (Decimal)
import Data.Fixed (E6, Fixed)
import qualified Data.Text as T
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import qualified Data.Vector as V
import Hledger (
  Status (Cleared, Pending),
  balassert,
  missingamt,
 )
import qualified Hledger as Ledger
import qualified Hledger.Data.Extra as HDE
import Hledger.Data.Lens (
  pBalanceAssertion,
  pStatus,
  tDescription,
  tStatus,
 )
import Hledupt.Data.CsvFile (CsvFile (..))
import Hledupt.Data.Currency (Currency (CHF))
import Hledupt.Data.Isin (Isin, mkIsin)
import Hledupt.Data.LedgerReport (LedgerReport (..))
import Hledupt.Data.MyDecimal (MyDecimal (unMyDecimal))
import Relude
import Relude.Unsafe (fromJust)

cashAccount :: Text
cashAccount = "Assets:Investments:Finpension:Cash"

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

instance CSV.FromField Category where
  parseField "Buy" = return Buy
  parseField "Deposit" = return Deposit
  parseField field =
    fail . decodeUtf8 $
      "Could not parse " <> field <> " as a category."

data RawTransaction = RawTransaction
  { rtDate :: !Day
  , rtCategory :: !Category
  , rtAssetName :: !Text
  , rtNumberOfShares :: !(Maybe Decimal)
  , rtAssetCurrency :: !Alpha
  , rtCurrencyRate :: !Decimal
  , rtAssetPriceInChf :: !(Maybe Decimal)
  , rtCashFlow :: !Decimal
  , rtBalance :: !Decimal
  }

newtype FinpensionDay = FinpensionDay
  { unFinpensionDay :: Day
  }

instance CSV.FromField FinpensionDay where
  parseField field =
    FinpensionDay
      <$> parseTimeM True defaultTimeLocale "%Y-%m-%d" (unpackStrict8 field)

instance CSV.FromNamedRecord RawTransaction where
  parseNamedRecord nr = do
    date <- unFinpensionDay <$> CSV.lookup nr "Date"
    cat <- CSV.lookup nr "Category"
    assetName <- CSV.lookup nr "Asset Name"
    nShares <- fmap unMyDecimal <$> CSV.lookup nr "Number of Shares"
    assetCurrency <- do
      alphaText <- CSV.lookup nr "Asset Currency"
      maybe
        (fail $ "Could not recognize " ++ alphaText ++ " as a currency.")
        return
        (readMaybe alphaText)
    currencyRate <- unMyDecimal <$> CSV.lookup nr "Currency Rate"
    assetPriceInChf <- fmap unMyDecimal <$> CSV.lookup nr "Asset Price in CHF"
    cashFlow <- unMyDecimal <$> CSV.lookup nr "Cash Flow"
    bal <- unMyDecimal <$> CSV.lookup nr "Balance"
    return $
      RawTransaction
        date
        cat
        assetName
        nShares
        assetCurrency
        currencyRate
        assetPriceInChf
        cashFlow
        bal

data Transaction = TrBuy BuyTransaction | TrDeposit DepositTransaction

data BuyTransaction = BuyTransaction
  { btDate :: !Day
  , btAssetName :: !Text
  , btNumberOfShares :: !(Fixed E6)
  , btAssetCurrency :: !Alpha
  , btCurrencyRate :: !(Fixed E6)
  , btAssetPriceInChf :: !(Fixed E6)
  , btCashFlow :: !(Fixed E6)
  , btBalance :: !(Fixed E6)
  }

data DepositTransaction = DepositTransaction
  { dtDate :: !Day
  , dtAssetCurrency :: !Alpha
  , dtCashFlow :: !Decimal
  , dtBalance :: !Decimal
  }

transactionsCsvDecodeOptions :: CSV.DecodeOptions
transactionsCsvDecodeOptions =
  CSV.defaultDecodeOptions
    { CSV.decDelimiter = fromIntegral (ord ';')
    }

rawTransactionsP :: CsvFile LByteString -> Either Text [RawTransaction]
rawTransactionsP (CsvFile csvContent) = do
  (_header, trsVector) <-
    over L._Left T.pack $
      CSV.decodeByNameWith transactionsCsvDecodeOptions csvContent
  return $ V.toList trsVector

rawTransactionToTransaction :: RawTransaction -> Transaction
rawTransactionToTransaction rawTr@(RawTransaction date Deposit _ _ _ _ _ _ _) =
  TrDeposit $
    DepositTransaction
      date
      (rtAssetCurrency rawTr)
      (rtCashFlow rawTr)
      (rtBalance rawTr)
rawTransactionToTransaction (RawTransaction date Buy _ _ _ _ _ _ _) =
  TrBuy $
    BuyTransaction
      date
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined

todoPosting :: Ledger.Posting
todoPosting =
  Ledger.post "Todo" missingamt
    & set pStatus Pending

finpensionTransactionToLedgerTransaction :: Transaction -> Ledger.Transaction
finpensionTransactionToLedgerTransaction (TrDeposit depositTr) =
  Ledger.transaction (dtDate depositTr) [depositPosting, todoPosting]
    & set tDescription "Finpension Deposit"
      . set tStatus Cleared
 where
  depositPosting =
    Ledger.post
      cashAccount
      (HDE.makeCurrencyAmount CHF . dtCashFlow $ depositTr)
      & set
        pBalanceAssertion
        (balassert . HDE.makeCurrencyAmount CHF . dtBalance $ depositTr)
finpensionTransactionToLedgerTransaction (TrBuy _buyTr) =
  Ledger.nulltransaction

rawTransactionToLedgerTransaction :: RawTransaction -> Ledger.Transaction
rawTransactionToLedgerTransaction = finpensionTransactionToLedgerTransaction . rawTransactionToTransaction

transactionsToLedger :: CsvFile LByteString -> Either Text LedgerReport
transactionsToLedger csv = do
  trs <- fmap rawTransactionToLedgerTransaction <$> rawTransactionsP csv
  return $ LedgerReport trs []
