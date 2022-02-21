module Hledupt.Splitwise (
  statementToLedger,
) where

import qualified Control.Lens as L
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as CSV
import Data.Either.Extra (mapLeft)
import qualified Data.Text as T
import Data.Time (Day)
import qualified Data.Vector as V
import Hledger (
  Status (Cleared),
  Transaction,
  missingamt,
  post,
 )
import Hledger.Data (balassert)
import Hledger.Data.Extra (
  ToPosting (..),
  makeCurrencyAmount,
  makeTransaction,
 )
import Hledger.Data.Lens (pBalanceAssertion)
import Hledupt.Data.CsvFile (CsvFile (unCsvFile))
import Hledupt.Data.Currency (Currency)
import Hledupt.Data.MyDecimal (MyDecimal (unMyDecimal))
import Hledupt.Wallet (debtAssets, (<:>))
import Relude

data CsvRow = CsvRow
  { fname :: !Text
  , lname :: !Text
  , amount :: !MyDecimal
  , currencyCode :: !Currency
  }
  deriving stock (Generic, Show)

instance CSV.FromRecord CsvRow

instance ToPosting CsvRow where
  toPosting (CsvRow firstName lastName amnt currency) =
    post (debtAssets <:> fullName) missingamt
      & L.set
        pBalanceAssertion
        (balassert . makeCurrencyAmount currency $ unMyDecimal amnt)
   where
    fullName = firstName <> (if T.null lastName then "" else " " <> lastName)

parseStatement :: CsvFile LBS.ByteString -> Either Text [CsvRow]
parseStatement = fmap V.toList . mapLeft toText . CSV.decode CSV.HasHeader . unCsvFile

parsedStatementToTransaction :: Day -> [CsvRow] -> Transaction
parsedStatementToTransaction day =
  makeTransaction
    day
    (Just Cleared)
    "Splitwise Balance"
    . fmap toPosting

statementToLedger :: Day -> CsvFile LBS.ByteString -> Either Text Transaction
statementToLedger day = fmap (parsedStatementToTransaction day) . parseStatement
