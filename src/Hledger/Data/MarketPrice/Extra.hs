module Hledger.Data.MarketPrice.Extra (showMarketPrice) where

import Data.Time.Format.ISO8601
  ( FormatExtension (ExtendedFormat),
    calendarFormat,
    formatShow,
  )
import Hledger (MarketPrice (MarketPrice))
import Relude
import Text.Printf (printf)

-- | Renders MarketPrice as Ledger-style text
--
-- @
-- P yyyy-mm-dd from price to
-- @
showMarketPrice :: MarketPrice -> String
showMarketPrice (MarketPrice day from to price) =
  printf
    "P %s %s %s %s\n"
    (formatShow (calendarFormat ExtendedFormat) day)
    from
    (show price :: String)
    to
