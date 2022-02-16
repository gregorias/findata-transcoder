module Hledupt.Data.LedgerReport (
  LedgerReport (..),
  showLedgerReport,
  todoPosting,
) where

import Hledger (
  Status (Pending),
  Transaction (tdate),
  missingamt,
  showTransaction,
 )
import qualified Hledger as Ledger
import Hledger.Data (MarketPrice)
import Hledger.Data.Lens (pStatus)
import Hledger.Data.MarketPrice.Extra (showMarketPrice)
import Relude
import Relude.Extra.Lens (set)

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
  fold (showTransaction <$> sortBy (compare `on` tdate) trs)
    <> fold (toText . showMarketPrice <$> mps)

-- | A posting that symbolizes an unknown pending transactions to be resolved.
todoPosting :: Ledger.Posting
todoPosting =
  Ledger.post "Todo" missingamt
    & set pStatus Pending
