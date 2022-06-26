module Hledger.Extra (
  showTransaction,
) where

import qualified Data.Text as T
import Hledger (Transaction)
import qualified Hledger as H
import Relude

-- | Formats a Transaction into Ledger format.
--
-- Unlike 'showTransaction' from Hledger, this function avoids unnecessary newlines.
showTransaction :: Transaction -> Text
showTransaction = (<> "\n") . T.strip . H.showTransaction
