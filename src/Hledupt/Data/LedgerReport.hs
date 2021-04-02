module Hledupt.Data.LedgerReport (
  LedgerReport (..),
  showLedgerReport,
) where

import qualified Data.Text as Text
import Hledger (Transaction (tdate), showTransaction)
import Hledger.Data (MarketPrice)
import Hledger.Data.MarketPrice.Extra (showMarketPrice)
import Relude

-- | This data type represents data gleaned from account statements (or other sources)
-- in a Ledger format
data LedgerReport = LedgerReport
  { ledgerReportTransactions :: [Transaction]
  , ledgerReportMarketPrices :: [MarketPrice]
  }
  deriving stock (Eq, Show)

instance Semigroup LedgerReport where
  (<>) (LedgerReport lts lmps) (LedgerReport rts rmps) =
    LedgerReport (lts <> rts) (lmps <> rmps)

instance Monoid LedgerReport where
  mempty = LedgerReport [] []

-- | Shows 'LedgerReport' in Ledger format
showLedgerReport :: LedgerReport -> Text
showLedgerReport (LedgerReport trs mps) =
  Text.concat (showTransaction <$> sortBy (compare `on` tdate) trs)
    `Text.append` Text.concat (Text.pack . showMarketPrice <$> mps)
