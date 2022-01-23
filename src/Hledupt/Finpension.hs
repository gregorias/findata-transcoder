{-# LANGUAGE OverloadedStrings #-}

-- | Parses Finpension's reports and produces Ledger reports.
module Hledupt.Finpension (
  funds,
  transactionsToLedger,
) where

import Control.Lens (each, over, set)
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as CSV
import Data.Currency (Alpha)
import Data.Decimal (Decimal)
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import qualified Data.Vector as V
import Hledger (
  AmountPrice (UnitPrice),
  Status (Cleared),
  amountSetFullPrecision,
  balassert,
  missingamt,
 )
import qualified Hledger as Ledger
import qualified Hledger.Data.Extra as HDE
import Hledger.Data.Lens (
  aAmountPrice,
  pBalanceAssertion,
  tDescription,
  tStatus,
 )
import Hledupt.Data.CsvFile (CsvFile (..))
import Hledupt.Data.Currency (chf)
import Hledupt.Data.Isin (Isin (unIsin), mkIsin)
import Hledupt.Data.LedgerReport (LedgerReport (..), todoPosting)
import Hledupt.Data.MyDecimal (MyDecimal (unMyDecimal))
import Hledupt.Wallet (financialServices)
import Relude
import Relude.Unsafe (fromJust)

cashAccount :: Text
cashAccount = "Assets:Investments:Finpension:Cash"

fundAccount :: Text -> Text
fundAccount shortName = "Assets:Investments:Finpension:" <> shortName

-- | Fund metadata. A mapping from an ISIN to the funds short name.
data Fund = Fund
  { fundIsin :: !Isin
  , fundShortName :: !Text
  }
  deriving stock (Show)

funds :: [Fund]
funds =
  fromJust $
    fmap (uncurry Fund)
      <$> L.traverseOf (each . L._1) mkIsin sourcelist
 where
  sourcelist =
    [
      ( "CH0130458182"
      , "World ex CH"
      )
    ,
      ( "CH0429081620"
      , "World ex CH"
      )
    ,
      ( "CH0214967314"
      , "World ex CH Small Cap"
      )
    ,
      ( "CH0017844686"
      , "Emerging Markets"
      )
    ,
      ( "CH0110869143"
      , "CH Small & Mid Cap"
      )
    ,
      ( "CH0033782431"
      , "CH Large Cap"
      )
    ]

findFundByIsin :: Isin -> Maybe Fund
findFundByIsin isin = find ((== isin) . fundIsin) funds

data Category = Buy | Deposit | Dividend | Fee | Sell
  deriving stock (Show, Eq)

instance CSV.FromField Category where
  parseField "Buy" = return Buy
  parseField "Deposit" = return Deposit
  parseField "Dividend" = return Dividend
  parseField "Flat-rate administrative fee" = return Fee
  parseField "Sell" = return Sell
  parseField field =
    fail . decodeUtf8 $
      "Could not parse " <> field <> " as a category."

data RawTransaction = RawTransaction
  { _rtDate :: !Day
  , _rtCategory :: !Category
  , rtAssetName :: !Text
  , rtIsin :: !(Maybe Isin)
  , _rtNumberOfShares :: !(Maybe Decimal)
  , rtAssetCurrency :: !Alpha
  , rtCurrencyRate :: !Decimal
  , _rtAssetPriceInChf :: !(Maybe Decimal)
  , rtCashFlow :: !Decimal
  , rtBalance :: !Decimal
  }
  deriving stock (Show)

newtype FinpensionDay = FinpensionDay
  { unFinpensionDay :: Day
  }

instance CSV.FromField FinpensionDay where
  parseField field =
    FinpensionDay
      <$> parseTimeM True defaultTimeLocale "%Y-%m-%d" (decodeUtf8 field)

instance CSV.FromNamedRecord RawTransaction where
  parseNamedRecord nr = do
    date <- unFinpensionDay <$> CSV.lookup nr "Date"
    cat <- CSV.lookup nr "Category"
    assetName <- CSV.lookup nr "Asset Name"
    isin <- CSV.lookup nr "ISIN"
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
        isin
        nShares
        assetCurrency
        currencyRate
        assetPriceInChf
        cashFlow
        bal

data Transaction
  = TrStock StockTransaction
  | TrDeposit DepositTransaction
  | TrDividend DividendTransaction
  | TrFee FeeTransaction

data StockTransaction = StockTransaction
  { btDate :: !Day
  , _btAssetName :: !Text
  , btIsin :: !Isin
  , btNumberOfShares :: !Decimal
  , _btAssetCurrency :: !Alpha
  , _btCurrencyRate :: !Decimal
  , btAssetPriceInChf :: !Decimal
  , btCashFlow :: !Decimal
  , btBalance :: !Decimal
  }

data DepositTransaction = DepositTransaction
  { dtDate :: !Day
  , _dtAssetCurrency :: !Alpha
  , dtCashFlow :: !Decimal
  , dtBalance :: !Decimal
  }

data DividendTransaction = DividendTransaction
  { divTrDate :: !Day
  , divTrAssetName :: !Text
  , _divTrIsin :: !Isin
  , _divTrAssetCurrency :: !Alpha
  , divTrCashFlow :: !Decimal
  , divTrBalance :: !Decimal
  }

data FeeTransaction = FeeTransaction
  { ftDate :: !Day
  , _ftAssetCurrency :: !Alpha
  , ftCashFlow :: !Decimal
  , ftBalance :: !Decimal
  }

transactionsCsvDecodeOptions :: CSV.DecodeOptions
transactionsCsvDecodeOptions =
  CSV.defaultDecodeOptions
    { CSV.decDelimiter = fromIntegral (ord ';')
    }

rawTransactionsP :: CsvFile LByteString -> Either Text [RawTransaction]
rawTransactionsP (CsvFile csvContent) = do
  (_header, trsVector) <-
    over L._Left toText $
      CSV.decodeByNameWith transactionsCsvDecodeOptions csvContent
  return $ V.toList trsVector

rawTransactionToTransaction :: RawTransaction -> Maybe Transaction
rawTransactionToTransaction
  rawTr@( RawTransaction
            date
            Buy
            _
            (Just isin)
            (Just nShares)
            _
            _
            (Just assetPriceInChf)
            _
            _
          ) =
    return $
      TrStock $
        StockTransaction
          date
          (rtAssetName rawTr)
          isin
          nShares
          (rtAssetCurrency rawTr)
          (rtCurrencyRate rawTr)
          assetPriceInChf
          (rtCashFlow rawTr)
          (rtBalance rawTr)
rawTransactionToTransaction
  rawTr@( RawTransaction
            date
            Sell
            _
            (Just isin)
            (Just nShares)
            _
            _
            (Just assetPriceInChf)
            _
            _
          ) =
    return $
      TrStock $
        StockTransaction
          date
          (rtAssetName rawTr)
          isin
          nShares
          (rtAssetCurrency rawTr)
          (rtCurrencyRate rawTr)
          assetPriceInChf
          (rtCashFlow rawTr)
          (rtBalance rawTr)
rawTransactionToTransaction (RawTransaction _ Buy _ _ _ _ _ _ _ _) = empty
rawTransactionToTransaction (RawTransaction _ Sell _ _ _ _ _ _ _ _) = empty
rawTransactionToTransaction rawTr@(RawTransaction date Deposit _ _ _ _ _ _ _ _) =
  return $
    TrDeposit $
      DepositTransaction
        date
        (rtAssetCurrency rawTr)
        (rtCashFlow rawTr)
        (rtBalance rawTr)
rawTransactionToTransaction rawTr@(RawTransaction date Dividend _ (Just isin) _ _ _ _ _ _) =
  return $
    TrDividend $
      DividendTransaction
        date
        (rtAssetName rawTr)
        isin
        (rtAssetCurrency rawTr)
        (rtCashFlow rawTr)
        (rtBalance rawTr)
rawTransactionToTransaction (RawTransaction _ Dividend _ _ _ _ _ _ _ _) = empty
rawTransactionToTransaction rawTr@(RawTransaction date Fee _ _ _ _ _ _ _ _) =
  return $
    TrFee $
      FeeTransaction
        date
        (rtAssetCurrency rawTr)
        (rtCashFlow rawTr)
        (rtBalance rawTr)

finpensionTransactionToLedgerTransaction :: Transaction -> Either Text Ledger.Transaction
finpensionTransactionToLedgerTransaction (TrDeposit depositTr) =
  return $
    Ledger.transaction (dtDate depositTr) [depositPosting, todoPosting]
      & set tDescription "Finpension Deposit"
        . set tStatus Cleared
 where
  depositPosting =
    Ledger.post
      cashAccount
      (HDE.makeCurrencyAmount chf . dtCashFlow $ depositTr)
      & set
        pBalanceAssertion
        (balassert . HDE.makeCurrencyAmount chf . dtBalance $ depositTr)
finpensionTransactionToLedgerTransaction (TrDividend divTr) =
  return $
    Ledger.transaction (divTrDate divTr) [divPosting, incomePosting]
      & set tDescription ("Finpension Dividend -- " <> divTrAssetName divTr)
        . set tStatus Cleared
 where
  divPosting =
    Ledger.post
      cashAccount
      ( (HDE.makeCurrencyAmount chf . divTrCashFlow $ divTr)
          & amountSetFullPrecision
      )
      & set
        pBalanceAssertion
        (balassert . amountSetFullPrecision . HDE.makeCurrencyAmount chf . divTrBalance $ divTr)
  incomePosting = Ledger.post "Income:Capital Gains" missingamt
finpensionTransactionToLedgerTransaction (TrFee feeTr) =
  return $
    Ledger.transaction (ftDate feeTr) [feePosting, financialServicesPosting]
      & set tDescription "Finpension Fee"
        . set tStatus Cleared
 where
  feePosting =
    Ledger.post
      cashAccount
      ( (HDE.makeCurrencyAmount chf . ftCashFlow $ feeTr)
          & amountSetFullPrecision
      )
      & set
        pBalanceAssertion
        (balassert . amountSetFullPrecision . HDE.makeCurrencyAmount chf . ftBalance $ feeTr)
  financialServicesPosting =
    Ledger.post
      financialServices
      ( (HDE.makeCurrencyAmount chf . negate . ftCashFlow $ feeTr)
          & amountSetFullPrecision
      )
finpensionTransactionToLedgerTransaction (TrStock buyTr) = do
  let isin = btIsin buyTr
  (Fund _ shortName) <-
    maybeToRight
      ( "Could not find fund " <> show isin <> " in registry."
          <> " You might need to add the fund to the registry."
      )
      (findFundByIsin isin)
  let fundPosting =
        Ledger.post
          (fundAccount shortName)
          ( ( (HDE.makeCommodityAmount (toText $ unIsin isin) . btNumberOfShares $ buyTr)
                & amountSetFullPrecision
            )
              & L.set
                aAmountPrice
                ( Just . UnitPrice $
                    HDE.makeCurrencyAmount chf (btAssetPriceInChf buyTr)
                      & amountSetFullPrecision
                )
          )
  return $
    Ledger.transaction (btDate buyTr) [cashPosting, fundPosting]
      & set tDescription "Finpension Purchase/Sell"
        . set tStatus Cleared
 where
  cashPosting =
    Ledger.post
      cashAccount
      ( (HDE.makeCurrencyAmount chf . btCashFlow $ buyTr)
          & amountSetFullPrecision
      )
      & set
        pBalanceAssertion
        ( balassert
            ( (HDE.makeCurrencyAmount chf . btBalance $ buyTr)
                & amountSetFullPrecision
            )
        )

rawTransactionToLedgerTransaction :: RawTransaction -> Either Text Ledger.Transaction
rawTransactionToLedgerTransaction rawTr = do
  finpensionTr <-
    maybe
      ( Left $
          "Could not recognize the following CSV transaction as a finpension transaction."
            <> " You might need to implement it. "
            <> show rawTr
      )
      Right
      (rawTransactionToTransaction rawTr)
  finpensionTransactionToLedgerTransaction finpensionTr

transactionsToLedger :: CsvFile LByteString -> Either Text LedgerReport
transactionsToLedger csv = do
  let bomLessCsv = dropBom <$> csv
  rawTrs <- sortChronologically <$> rawTransactionsP bomLessCsv
  trs <- sequence $ rawTransactionToLedgerTransaction <$> rawTrs
  return $ LedgerReport trs []
 where
  sortChronologically = reverse
  dropBom :: LByteString -> LByteString
  dropBom = LBS.drop 3
