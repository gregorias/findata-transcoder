-- | This module parses Degiro's Portfolio CSV statement into Ledger.
module Hledupt.Degiro.Portfolio (
  csvStatementToLedger,
) where

import Control.Lens (Prism', over, preview, prism', set, view)
import qualified Control.Lens as L
import qualified Data.ByteString as BS
import Data.ByteString.Internal (c2w)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as Csv
import Data.Decimal (Decimal)
import Data.Time (Day)
import Data.Vector (Vector)
import Hledger (MarketPrice (MarketPrice), Status (Cleared), balassert, post, transaction)
import Hledger.Data (Transaction)
import Hledger.Data.Extra (ToPosting (..), makeCashAmount, makeCommodityAmount)
import Hledger.Data.Lens (pBalanceAssertion, tDescription, tStatus)
import Hledupt.Data.Cash (Cash (..))
import Hledupt.Data.CsvFile (CsvFile (..))
import Hledupt.Data.Isin (Isin, mkIsin)
import Hledupt.Data.LedgerReport (LedgerReport (..))
import Hledupt.Data.MyDecimal (myDecDec)
import Hledupt.Degiro.IsinData (IsinData (IsinData), isinToIsinData, prettyIsin)
import Relude

data IsinOrEmpty = IsinOrEmptyIsin Isin | IsinOrEmptyEmpty
  deriving stock (Show)

instance Csv.FromField IsinOrEmpty where
  parseField "" = pure IsinOrEmptyEmpty
  parseField field = case mkIsin . decodeUtf8 $ field of
    Just isin -> return $ IsinOrEmptyIsin isin
    Nothing -> mzero

data AmountOrEmpty = AmountOrEmptyAmount Integer | AmountOrEmptyEmpty
  deriving stock (Show)

instance Csv.FromField AmountOrEmpty where
  parseField "" = pure AmountOrEmptyEmpty
  parseField field = case readMaybe . decodeUtf8 $ field of
    Just amt -> return . AmountOrEmptyAmount $ amt
    Nothing -> mzero

data DecimalOrEmpty = DecimalOrEmptyDecimal Decimal | DecimalOrEmptyEmpty
  deriving stock (Show)

instance Csv.FromField DecimalOrEmpty where
  parseField "" = pure DecimalOrEmptyEmpty
  parseField field =
    DecimalOrEmptyDecimal . view myDecDec
      <$> Csv.parseField field

data CsvRecord = CsvRecord
  { _crProduct :: Text
  , _crIsin :: IsinOrEmpty
  , _crAmount :: AmountOrEmpty
  , _crClosing :: DecimalOrEmpty
  , _crLocalValue :: Cash
  }
  deriving stock (Show)

newtype DegiroPortfolioCash = DegiroPortfolioCash
  { getDegiroPortfolioCash :: Cash
  }

instance Csv.FromField DegiroPortfolioCash where
  parseField field =
    DegiroPortfolioCash
      <$> ( Cash
              <$> Csv.parseField currency
              <*> (view myDecDec <$> Csv.parseField (BS.drop 1 value))
          )
   where
    (currency, value) = BS.break (== c2w ' ') field

instance Csv.FromNamedRecord CsvRecord where
  parseNamedRecord nr = do
    productName <- Csv.lookup nr "Product"
    isin <- Csv.lookup nr "Symbol/ISIN"
    amt <- Csv.lookup nr "Amount"
    closing <- Csv.lookup nr "Closing"
    localVal <- getDegiroPortfolioCash <$> Csv.lookup nr "Local value"
    return $
      CsvRecord
        productName
        isin
        amt
        closing
        localVal

newtype CashPosition = CashPosition Cash

instance ToPosting CashPosition where
  toPosting (CashPosition cash@(Cash currency _value)) =
    post
      "Assets:Liquid:Degiro"
      (makeCashAmount (Cash currency 0))
      & L.set pBalanceAssertion (balassert $ makeCashAmount cash)

data StockPosition = StockPosition
  { _spIsin :: Isin
  , _spAmount :: Integer
  , _spPrice :: Decimal
  }

instance ToPosting StockPosition where
  toPosting (StockPosition isin amount _price) =
    post
      ("Assets:Investments:Degiro:" <> name)
      (makeCommodityAmount name 0)
      & L.set
        pBalanceAssertion
        ( balassert $
            makeCommodityAmount
              name
              (fromInteger amount)
        )
   where
    name = prettyIsin isin

stockPositionToMarketPrice :: Day -> StockPosition -> Maybe MarketPrice
stockPositionToMarketPrice stmtDate (StockPosition isin _amt price) = do
  IsinData symbol currency <- isinToIsinData isin
  return $ MarketPrice stmtDate symbol (toText $ show @String currency) price

data Position = PositionCash CashPosition | PositionStock StockPosition

_PositionStock :: Prism' Position StockPosition
_PositionStock = prism' PositionStock foo
 where
  foo (PositionStock sp) = Just sp
  foo _ = Nothing

instance ToPosting Position where
  toPosting (PositionCash cp) = toPosting cp
  toPosting (PositionStock sp) = toPosting sp

csvRecordToPosition :: CsvRecord -> Maybe Position
csvRecordToPosition
  ( CsvRecord
      _product
      IsinOrEmptyEmpty
      AmountOrEmptyEmpty
      DecimalOrEmptyEmpty
      localValue
    ) = Just . PositionCash $ CashPosition localValue
csvRecordToPosition
  ( CsvRecord
      _product
      (IsinOrEmptyIsin isin)
      (AmountOrEmptyAmount amt)
      (DecimalOrEmptyDecimal closing)
      _localValue
    ) = Just . PositionStock $ StockPosition isin amt closing
csvRecordToPosition _ = Nothing

positionsToStatusTransaction ::
  (Foldable t, Functor t, ToPosting a) =>
  Day ->
  t a ->
  Transaction
positionsToStatusTransaction stmtDate positions =
  transaction
    stmtDate
    (toList $ fmap toPosting positions)
    & set tStatus Cleared
      . set tDescription "Degiro Status"

-- | Transforms a Degiro CSV statement into a Ledger report
csvStatementToLedger :: Day -> CsvFile LBS.ByteString -> Either Text LedgerReport
csvStatementToLedger stmtDate (CsvFile csvFile) = do
  csvRecords :: Vector CsvRecord <-
    over L._Right snd $
      over L._Left toText $ Csv.decodeByName csvFile
  positions <-
    maybe
      (Left "Could not transform csv records to positions.\n")
      Right
      (traverse csvRecordToPosition csvRecords)
  marketPrices <-
    maybe
      (Left "Could not transform positions to prices.\n")
      Right
      ( mapM
          (stockPositionToMarketPrice stmtDate)
          ( catMaybes . toList $
              preview _PositionStock <$> positions
          )
      )
  return $
    LedgerReport
      [positionsToStatusTransaction stmtDate positions]
      marketPrices
